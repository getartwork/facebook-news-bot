package com.gu.facebook_news_bot

import com.gu.contentapi.client.model.v1.Content
import com.gu.facebook_news_bot.services.{Capi, Topic}
import com.gu.facebook_news_bot.util.JsonHelpers
import com.gu.contentapi.json.CirceDecoders._

import scala.concurrent.Future

object DummyCapi extends Capi {

  def getHeadlines(edition: String, topic: Option[Topic]): Future[Seq[Content]] = {
    getFromFile("headlines", edition, topic)
  }

  def getMostViewed(edition: String, topic: Option[Topic]): Future[Seq[Content]] = {
    getFromFile("mostviewed", edition, topic)
  }

  private def getFromFile(`type`: String, edition: String, topic: Option[Topic]): Future[Seq[Content]] = {
    val file = s"src/test/resources/capiResponses/${`type`}-${topic.map(_.getQuery(edition).pathSegment.replace("/","-")).getOrElse(edition)}.json"
    val result = JsonHelpers.decodeFromFile[Seq[Content]](file)

    Future.successful(result)
  }
}
