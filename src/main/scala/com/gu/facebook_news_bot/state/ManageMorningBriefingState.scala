package com.gu.facebook_news_bot.state

import com.gu.facebook_news_bot.models.{MessageFromFacebook, MessageToFacebook, User}
import com.gu.facebook_news_bot.services.{Capi, Facebook}
import com.gu.facebook_news_bot.state.StateHandler.Result
import com.gu.facebook_news_bot.stores.UserStore
import com.gu.facebook_news_bot.utils.ResponseText

import scala.concurrent.Future

object ManageMorningBriefingState extends State {
  val Name = "MANAGE_MORNING_BRIEFING"

  def transition(user: User, messaging: MessageFromFacebook.Messaging, capi: Capi, facebook: Facebook, store: UserStore): Future[Result] = {
    State.getUserInput(messaging).flatMap { text =>
      val lower = text.toLowerCase
      if (lower.contains("time")) Some(BriefingTimeQuestionState.question(user))
      else if (lower.contains("unsubscribe")) Some(MainState.unsubscribe(user))
      else None
    } getOrElse State.unknown(user)
  }

  def question(user: User): Future[Result] = {
    if (user.notificationTime == "-") {
      SubscribeQuestionState.question(user)
    } else {
      val quickReplies = Seq(
        MessageToFacebook.QuickReply(title = Some("Change time"), payload = Some("time")),
        MessageToFacebook.QuickReply(title = Some("Unsubscribe"), payload = Some("unsubscribe"))
      )

      val message = MessageToFacebook.quickRepliesMessage(
        user.ID,
        quickReplies,
        ResponseText.manageSubscription(EditionQuestionState.frontToUserFriendly(user.front), user.notificationTime)
      )

      Future.successful((State.changeState(user, Name), List(message)))
    }
  }
}
