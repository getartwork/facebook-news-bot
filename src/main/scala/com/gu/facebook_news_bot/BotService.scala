package com.gu.facebook_news_bot

import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.actor.ActorSystem
import akka.http.scaladsl.model.{HttpEntity, HttpResponse, StatusCodes}
import akka.http.scaladsl.server.{ExceptionHandler, RejectionHandler}
import akka.stream.{ActorMaterializer, Materializer}
import akka.http.scaladsl.unmarshalling.{PredefinedFromEntityUnmarshallers, Unmarshal}
import com.amazonaws.regions.Regions
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBAsyncClient
import com.amazonaws.services.dynamodbv2.model.ConditionalCheckFailedException
import com.gu.cm.Mode
import com.gu.facebook_news_bot.briefing.MorningBriefingPoller
import com.gu.facebook_news_bot.models.{MessageFromFacebook, MessageToFacebook, User}
import com.gu.facebook_news_bot.services._
import de.heikoseeberger.akkahttpcirce.CirceSupport
import io.circe.generic.auto._
import com.gu.facebook_news_bot.state.StateHandler
import com.gu.facebook_news_bot.stores.UserStore
import com.gu.facebook_news_bot.utils._
import com.gu.facebook_news_bot.utils.Loggers._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.{Failure, Success}

trait BotService extends CirceSupport with PredefinedFromEntityUnmarshallers {
  implicit val system: ActorSystem
  implicit def executor: ExecutionContextExecutor
  implicit val materializer: Materializer

  val dynamoClient: AmazonDynamoDBAsyncClient
  val capi: Capi
  val facebook: Facebook
  val stateHandler: StateHandler
  val usersTable: String

  lazy val userStore = new UserStore(dynamoClient, usersTable)

  implicit val rejectionHandler = RejectionHandler.newBuilder()
    .handle { case r =>
      appLogger.warn(s"Rejected request: $r")
      //Send a 200 because otherwise FB refuses to send any more messages from any users
      complete(StatusCodes.OK)
    }
    .result()

  implicit val exceptionHandler: ExceptionHandler =
    ExceptionHandler {
      case error =>
        appLogger.warn(s"Exception in route execution: ${error.getMessage}", error)
        complete(StatusCodes.OK)
    }

  val routes = path("status") {
    get {
      complete("OK")
    }
  } ~ path("webhook") {
    post {
      extractRequest { request =>
        appLogger.debug(s"Received new webhook POST: $request")

        optionalHeaderValueByName("X-Hub-Signature") { signature =>
          //Ensure we have the full entity
          onComplete(request.entity.toStrict(5.seconds)) {

            case Success(strictEntity) =>
              val bytes = strictEntity.getData.toArray
              if (BotConfig.stage == Mode.Dev || signature.exists(Verification.verifySignature(_, bytes))) {
                getMessagings(strictEntity).onComplete {
                  case Success(messagings) =>
                    messagings foreach { messaging =>
                      //For now, ignore message receipts
                      if (messaging.message.isDefined || messaging.postback.isDefined) processMessaging(messaging)
                    }
                  case Failure(error) => appLogger.error(s"Failed to unmarshal messagings: ${error.getMessage}", error)
                }
              } else appLogger.warn(s"Invalid X-Hub-Signature header, rejecting POST request.")

              complete(HttpResponse(StatusCodes.OK))

            case Failure(error) =>
              appLogger.error(s"Failed to get whole of entity: ${error.getMessage}", error)

              complete(HttpResponse(StatusCodes.OK))
          }
        }
      }
    }
  }

  /**
    * A message from FB may contains many 'entries', each with many 'messagings', from any number of users.
    * The order in which they are processed is not important.
    * We respond immediately with a 200.
    */
  private def getMessagings(strictEntity: HttpEntity.Strict): Future[Seq[MessageFromFacebook.Messaging]] = {
    Unmarshal(strictEntity).to[MessageFromFacebook] map { fbMessage =>
      val messagings: Seq[MessageFromFacebook.Messaging] = for {
        entry <- fbMessage.entry
        messaging <- entry.messaging
      } yield messaging

      messagings
    }
  }

  case class ReceiptLog(event: String, messaging: MessageFromFacebook.Messaging, user: Option[User])
  case class CompleteLog(event: String, messages: List[MessageToFacebook], user: Option[User])

  private def processMessaging(msg: MessageFromFacebook.Messaging, retry: Int = 0): Unit = {

    val futureResult = for {
      user <- userStore.getUser(msg.sender.id)
      result <- stateHandler.process(user, msg)
    } yield {
      logEvent(JsonHelpers.encodeJson(ReceiptLog("receipt", msg, user)))
      result
    }

    futureResult onComplete {
      case Success((updatedUser, messages)) =>
        /**
          * Update user state in dynamo, then send messages
          */
        userStore.updateUser(updatedUser) foreach { userUpdateResult =>
          userUpdateResult.fold({ error: ConditionalCheckFailedException =>
            //User has since been updated in dynamo, try again with the latest version
            if (retry < 3) processMessaging(msg, retry+1)
            else {
              //Something has gone very wrong
              appLogger.error(s"Failed to update user state multiple times. User is $updatedUser and error is ${error.getMessage}", error)
            }
          }, { _ =>
            logEvent(JsonHelpers.encodeJson(CompleteLog("complete", messages, Some(updatedUser))))
            facebook.send(messages)
          })
        }
      case Failure(error) =>
        appLogger.error(s"Error processing message $msg: ${error.getMessage}", error)
        facebook.send(List(MessageToFacebook.errorMessage(msg.sender.id)))
    }
  }
}

object Bot extends App with BotService {
  override implicit val system = ActorSystem("facebook-news-bot-actor-system")
  override implicit val executor = system.dispatcher
  override implicit val materializer = ActorMaterializer()

  override val capi = CapiImpl
  override val facebook = new FacebookImpl()
  override val stateHandler = StateHandler(facebook, capi)
  override val usersTable = BotConfig.aws.usersTable

  override val dynamoClient: AmazonDynamoDBAsyncClient = {
    if (BotConfig.stage == Mode.Dev) {
      new AmazonDynamoDBAsyncClient().withEndpoint("http://localhost:8000")
    } else {
      val awsRegion = Regions.fromName(BotConfig.aws.region)
      new AmazonDynamoDBAsyncClient(BotConfig.aws.CredentialsProvider).withRegion(awsRegion)
    }
  }

  BotConfig.aws.loggingKinesisStreamName match {
    case Some(stream) =>
      val appenderConfig = KinesisAppenderConfig(stream, BotConfig.aws.CredentialsProvider)
      LogStash.init(appenderConfig)
    case None => appLogger.warn(s"No kinesis stream name found for logging.")
  }

  val poller = PartialFunction.condOpt(BotConfig.stage != Mode.Dev) {
    case true => system.actorOf(MorningBriefingPoller.props(userStore, capi, facebook))
  }

  Parser.warmUp

  val bindingFuture = Http().bindAndHandle(routes, "0.0.0.0", BotConfig.port)
}
