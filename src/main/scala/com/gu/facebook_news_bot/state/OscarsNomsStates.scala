package com.gu.facebook_news_bot.state

import com.gu.facebook_news_bot.models.{MessageFromFacebook, MessageToFacebook, User, UserNoms}
import com.gu.facebook_news_bot.services.{Capi, Facebook}
import com.gu.facebook_news_bot.state.StateHandler.Result
import com.gu.facebook_news_bot.stores.UserStore
import com.gu.facebook_news_bot.utils.Loggers.LogEvent
import io.circe.generic.auto._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object OscarsNomsStates {

  case object InitialQuestionState extends YesOrNoState {

    val Name = "OSCARS_NOMS_INITIAL_QUESTION"

    private case class NoEvent(id: String, event: String = "oscars_noms_subscribe_no", _eventName: String = "oscars_noms_subscribe_no") extends LogEvent

    val Question = "Would you like to play our Oscars Predictions game?"

    protected def getQuestionText(user: User) = {
      if (user.version.contains(0)) s"Hi, I'm the Guardian chatbot. $Question"
      else Question
    }

    protected def yes(user: User, facebook: Facebook): Future[Result] = EnterNomsState.question(user)

    protected def no(user: User): Future[Result] = {
      State.log(NoEvent(id = user.ID))
      MainState.menu(user, "Ok. Is there anything else I can help you with?")
    }

  }

  case object EnterNomsState extends State {

    val Name = "OSCARS_ENTER_NOMS"

    private val NoPattern = """\b(no|nope|nah|not)\b""".r.unanchored

    private case class NewSubscriberEvent(id: String, event: String = "oscars_noms_subscribe", _eventName: String = "oscars_noms_subscribe") extends LogEvent

    def transition(user: User, messaging: MessageFromFacebook.Messaging, capi: Capi, facebook: Facebook, store: UserStore): Future[Result] = {
      State.getUserInput(messaging) match {
        case Some(text) => enterPredictions(user, store, text)
        case None => notPlaying(user)
      }
    }

    def question(user: User, text: Option[String] = None): Future[Result] = {
      requestPrediction("BEST_PICTURE", user)
      //val message = MessageToFacebook.textMessage(user.ID, text.getOrElse("OK, which film do you think will win Best Picture?"))
      //Future.successful(State.changeState(user, Name), List(message))
    }

    override def onPostback(...): Future[Result] = {
      //get UserNom
      //check postback data is valid
      //update UserNom with new prediction
      // check if prediction is in list, and send next element as category
      // list.isEmpty => updateState
      //requestPrediction(cat)
    }

    def requestPrediction(category: String, user: User, userNoms: UserNoms): Future[Result] = {
      // if start of list, send specific message, else confirm & ask.
      //build carousel
      // confirm nomination

      Future.successful(State.changeState(user, Name), Nil)
    }

    def enterPredictions(user: User, store: UserStore, text: String): Future[Result] = {
     val predictions = store.OscarsStore.getUserNominations(user.ID)
      predictions.flatMap { result =>
        if (result.isEmpty) {
          isPlaying(user, text, store)
        } else {
          notPlaying(user)
        }
      }
    }

    private def isPlaying(user: User, text: String, store: UserStore): Future[Result] = {
      text.toLowerCase match {
        case YesOrNoState.YesPattern(_) => question(user)
        case NoPattern(_) => question(user, Some("Sorry I didn't get that, could you please repeat?"))
        case _ => {
          store.OscarsStore. putUserNominations( UserNoms(user.ID, Some(text)) )
          val updatedUser = {
            if (!user.oscarsNoms.contains(true)) {
              State.log(NewSubscriberEvent(user.ID))
              user.copy(
                oscarsNoms = Some(true)
              )
            } else user
          }
          question(
            updatedUser,
            Some(s"You guessed ${text} for Best Picture."))
        }

      }
    }

    private def notPlaying(user: User): Future[Result] = question(user, Some("Is there anything else I can help you with?"))

  }

}