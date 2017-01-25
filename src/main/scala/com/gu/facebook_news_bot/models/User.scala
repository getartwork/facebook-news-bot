package com.gu.facebook_news_bot.models

/**
  * The user data as it is stored in dynamodb
  */
case class User(ID: String,
                front: String,
                offsetHours: Double,
                notificationTime: String,
                notificationTimeUTC: String,
                state: Option[String] = None,
                version: Option[Long] = None,
                contentTopic: Option[String] = None,
                contentOffset: Option[Int] = None,
                contentType: Option[String] = None,
                daysUncontactable: Option[Int] = None,
                footballTransfers: Option[Boolean] = None,
                footballRumoursTimeUTC: Option[String] = None,
                oscarsNoms: Option[Boolean] = None,
                oscarsNomsUpdateType: Option[Boolean] = None)
