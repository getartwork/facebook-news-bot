package com.gu.facebook_news_bot.state

import com.gu.facebook_news_bot.models.{MessageFromFacebook, MessageToFacebook, User}
import com.gu.facebook_news_bot.services.Facebook.{GetUserResult, GetUserSuccessResponse}
import com.gu.facebook_news_bot.services.{Capi, Facebook}
import com.gu.facebook_news_bot.state.StateHandler.{ReferralEvent, Result}
import com.gu.facebook_news_bot.stores.UserStore
import com.gu.facebook_news_bot.utils.Loggers.LogEvent

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import io.circe.generic.auto._

object StateHandler {
  def apply(facebook: Facebook, capi: Capi, store: UserStore) = new StateHandler(facebook, capi, store)

  type Result = (User, List[MessageToFacebook])

  val NewUserStateName = "NEW_USER"

  private case class ReferralEvent(id: String, event: String = "referral", _eventName: String = "referral", referrer: String) extends LogEvent

  private val States: Map[String, State] = List(
    MainState,
    SubscribeQuestionState,
    BriefingTimeQuestionState,
    EditionQuestionState,
    FeedbackState,
    ManageMorningBriefingState,
    FootballTransferStates.InitialQuestionState,
    FootballTransferStates.EnterTeamsState,
    FootballTransferStates.ManageFootballTransfersState,
    FootballTransferStates.RemoveTeamState,
    OscarsNomsStates.InitialQuestionState,
    OscarsNomsStates.EnterNomsState,
    UnsubscribeState)
    .map((state: State) => (state.Name, state))
    .toMap + (StateHandler.NewUserStateName -> SubscribeQuestionState)

  private def getStateFromString(state: String): State = States.getOrElse(state.toUpperCase, MainState)

  private def localToFront(locale: String): String = {
    locale match {
      case "en_GB" => "uk"
      case "en_US" => "us"
      case "en_UD" => "au"
      case _ => "international"
    }
  }
}

class StateHandler(facebook: Facebook, capi: Capi, store: UserStore) {

  /**
    * @param userOpt  user from dynamodb
    * @param message  message from Facebook messenger
    * @return updated user, plus any messages to send to Facebook
    */
  def process(userOpt: Option[User], message: MessageFromFacebook.Messaging): Future[Result] = {
    userOpt.map(Future.successful).getOrElse(newUser(message.sender.id)) flatMap { user =>
      message.postback.map(postback => processPostback(postback, user))
        .orElse(message.referral.flatMap(referral => processReferral(referral, user)))
        .getOrElse {
          if (user.state.contains(StateHandler.NewUserStateName)) SubscribeQuestionState.question(user)
          else {
            val state = user.state.map(StateHandler.getStateFromString) getOrElse MainState
            state.transition(user, message, capi, facebook, store)
          }
        }
    }
  }

  /**
    * A postback with the "start" payload indicates either:
    * 1. A new user, who may or may not have been referred from somewhere
    * 2. An existing user who has been referred from somewhere (e.g. the football transfers interactive page)
    *
    * Any other kind of postback will be a button click, and should be handled in the MAIN state.
    */
  private def processPostback(postback: MessageFromFacebook.Postback, user: User): Future[Result] = {
    if (postback.payload.toLowerCase.contains("start")) {
      postback.referral.flatMap(ref => processReferral(ref, user))
        .orElse(user.state.collect { case StateHandler.NewUserStateName => SubscribeQuestionState.question(user) })
        .getOrElse(State.greeting(user))
    } else MainState.onMenuButtonClick(user, postback, capi, facebook, store)
  }

  /**
    * A referral field may be present inside or outside of a postback.
    * Either way, we want to log the referrer, and potentially update the state based on the referrer.
    */
  private def processReferral(referral: MessageFromFacebook.Referral, user: User): Option[Future[Result]] = {
    State.log(ReferralEvent(id = user.ID, referrer = referral.ref))
    referral.ref match {
      case "football_transfers" => Some(FootballTransferStates.InitialQuestionState.question(user))
      case "oscars_noms" => Some(OscarsNomsStates.InitialQuestionState.question(user))
      case _ => None
    }
  }

  case class GetUserException(data: GetUserResult) extends Throwable
  private def newUser(id: String): Future[User] = {
    facebook.getUser(id) map {
      case GetUserSuccessResponse(facebookUser) => User(id, StateHandler.localToFront(facebookUser.locale), facebookUser.timezone, "-", "-", Some(StateHandler.NewUserStateName), Some(0))
      case other =>
        //Facebook won't give us the user's data for whatever reason. Throwing an exception here will cause the standard error response to be sent.
        throw GetUserException(other)
    }
  }

  private def getStateFromString(state: String): State = state.toUpperCase match {
    case StateHandler.NewUserStateName => SubscribeQuestionState
    case SubscribeQuestionState.Name => SubscribeQuestionState
    case BriefingTimeQuestionState.Name => BriefingTimeQuestionState
    case EditionQuestionState.Name => EditionQuestionState
    case FeedbackState.Name => FeedbackState
    case ManageMorningBriefingState.Name => ManageMorningBriefingState
    case FootballTransferStates.InitialQuestionState.Name => FootballTransferStates.InitialQuestionState
    case FootballTransferStates.EnterTeamsState.Name => FootballTransferStates.EnterTeamsState
    case FootballTransferStates.ManageFootballTransfersState.Name => FootballTransferStates.ManageFootballTransfersState
    case FootballTransferStates.RemoveTeamState.Name => FootballTransferStates.RemoveTeamState
    case OscarsNomsStates.InitialQuestionState.Name => OscarsNomsStates.InitialQuestionState
    case OscarsNomsStates.EnterNomsState.Name => OscarsNomsStates.EnterNomsState
    case UnsubscribeState.Name => UnsubscribeState
    case _ => MainState
  }

  private def localToFront(locale: String): String = {
    locale match {
      case "en_GB" => "uk"
      case "en_US" => "us"
      case "en_UD" => "au"
      case _ => "international"
    }
  }

}
