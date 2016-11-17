package com.gu.facebook_news_bot

import com.gu.facebook_news_bot.services.SearchTopic
import org.scalatest.{FunSpec, Matchers}

class SearchTopicTest extends FunSpec with Matchers {
  it("should extract Clinton") {
    val topic = SearchTopic("What is your latest News on Clinton?")
    topic.get.terms should be(List("Clinton"))
  }

  it("should extract microsoft, apple and twitter") {
    val topic = SearchTopic("news on boring microsoft and tedious apple, oh and also twitter. I hate them all")
    topic.get.terms should be(List("microsoft", "apple", "twitter"))
  }

  it("should extract clinton, trump, election") {
    val topic = SearchTopic("did clinton or trump win the election?")
    topic.get.terms should be(List("clinton","trump","election"))
  }

  it("should extract Obama, Trump") {
    val topic = SearchTopic("Obama and Trump")
    topic.get.terms should be(List("Obama", "Trump"))
  }

  it("should extract Donald Trump") {
    val topic = SearchTopic("Donald Trump?")
    topic.get.terms should be(List("Donald Trump"))
  }

  it("should extract the election") {
    val topic = SearchTopic("tell me about the election")
    topic.get.terms should be(List("election"))
  }
}
