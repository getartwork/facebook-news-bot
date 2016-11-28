package com.gu.facebook_news_bot.utils

import scala.util.Random

object ResponseText {
  def greeting = {
    val start = random(List(
      "Hi there",
      "Hi",
      "Hello",
      "Hey"
    ))
    s"$start. How can I help?"
  }

  def thanksResponse = random(List(
      "No worries",
      "You're welcome",
      "Anytime"
    ))

  def goodbyeResponse = random(List(
    "Bye",
    "Goodbye"
  ))

  def welcome = "Hi, I'm the Guardian chatbot. I'll keep you up-to-date with the latest news.\n\nWould you like me to deliver a daily morning briefing to you?"

  def unknown = "Sorry, I didn't understand that. Are any of these helpful?"

  def noResults = "Sorry, I don't have any stories on that at the moment"

  def menu = "How can I help?"

  def briefingTimeQuestion = "When would you like your morning briefing delivered? You can choose from 7, 8 or 9am."

  def subscribeQuestion = "Would you like to subscribe to the daily morning briefing?"

  def subscribed(time: String) = s"Done. You will start receiving the morning briefing at $time.\n\nRemember, you can change your subscription to this at any time from the menu.\n\nWould you like to see the headlines or the most popular stories right now?"

  def subscribeNo = "No problem, maybe later then. You can subscribe to the morning briefing at any time from the menu.\n\nWould you like the headlines or the most popular stories?"

  def unsubscribe = "Done. You will no longer receive the morning briefing.\n\nYou can re-subscribe at any time from the menu"

  def manageSubscription(edition: String, time: String) = s"Your edition is currently set to $edition and your morning briefing time is $time.\n\nWhat would you like to change?"

  def editionQuestion = "Please choose a new edition:"

  def editionChanged(edition: String) = s"Your edition has been updated to $edition"

  def morningBriefing = random(List(
    "Good morning! Here are the top stories today",
    "Good morning! Your briefing is ready for you",
    "Good morning! Your briefing has arrived",
    "Good morning! Check out this morning's headline stories"
  ))

  def suggest = "You can ask for headlines or the most popular stories for various topics. Here are some examples:"

  def support = "Producing well-reported journalism is difficult and expensive. Supporting us isn't. Please support the Guardian."

  private def random(list: List[String]) = list((Random.nextDouble * list.length).floor.toInt)
}
