package com.gu.facebook_news_bot.state

import com.gu.facebook_news_bot.models.{Id, MessageFromFacebook, MessageToFacebook, User}
import com.gu.facebook_news_bot.services.{Capi, Facebook}
import com.gu.facebook_news_bot.state.StateHandler.Result
import com.gu.facebook_news_bot.utils.ResponseText
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * When a user asks to subscribe to the morning briefings, they enter this state.
  * It prompts them to specify one of the valid times.
  * Upon receiving a valid time, the notification time is updated in dynamo and the user enters the MAIN state.
  */
case object BriefingTimeQuestionState extends State {
  val Name = "BRIEFING_TIME_QUESTION"

  val ValidTimes = Seq("6", "7", "8")

  def transition(user: User, messaging: MessageFromFacebook.Messaging, capi: Capi, facebook: Facebook): Future[Result] = {
    messaging.postback.map(MainState.onMenuButtonClick(user, _, capi, facebook)) getOrElse {
      if (user.state.contains(Name)) {
        //There should be valid time in either the text or quick_reply fields
        val maybeTime = ValidTimes.find(time => State.getUserInput(messaging).exists(u => u.contains(time)))
        maybeTime.map(time => success(user, time, facebook)).getOrElse(question(user))
      } else {
        //New state
        question(user)
      }
    }
  }

  //Ask the user what time they'd like the briefing
  def question(user: User): Future[Result] = {
    val replies = ValidTimes.map(t => MessageToFacebook.QuickReply("text", Some(s"${t}am"), Some(s"$t")))
    val message = MessageToFacebook.quickRepliesMessage(user.ID, replies, ResponseText.briefingTimeQuestion)
    Future.successful((State.changeState(user, Name), List(message)))
  }

  private def success(user: User, time: String, facebook: Facebook): Future[Result] = {
    facebook.getUser(user.ID) flatMap { fbData =>
      val notifyTime = DateTime.parse(time, DateTimeFormat.forPattern("H"))
      val notifyTimeUTC = notifyTime.minusMinutes((fbData.timezone * 60).toInt)

      val message = MessageToFacebook.textMessage(user.ID, ResponseText.subscribed(time))
      val updatedUser = user.copy(
        state = Some(MainState.Name),
        notificationTime = notifyTime.toString("HH:mm"),
        notificationTimeUTC = notifyTimeUTC.toString("HH:mm")
      )
      Future.successful(updatedUser, List(message))
    }
  }
}
