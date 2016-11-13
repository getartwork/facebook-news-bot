package com.gu.facebook_news_bot.util

import cats.data.Xor
import com.gu.facebook_news_bot.models.MessageToFacebook
import io.circe.{Decoder, DecodingFailure, Json}

import scala.io.Source._
import io.circe.parser._
import io.circe.generic.auto._

object JsonHelpers {
  def loadFile(path: String): String = fromFile(path).mkString

  def loadJson(path: String): Json = parse(loadFile(path)).getOrElse(sys.error(s"Error parsing $path"))

  def decodeFromFile[T : Decoder](path: String): T = loadJson(path).as[T].getOrElse(sys.error(s"Error decoding $path"))

  implicit def messagesDecoder: Decoder[Seq[MessageToFacebook]] = Decoder.instance { cursor =>
    val result = cursor.focus.asArray.map { array =>
      array.flatMap { item =>
        item.as[MessageToFacebook].toOption
      }
    }
    Xor.fromOption(result, DecodingFailure("No array found", cursor.history))
  }
}
