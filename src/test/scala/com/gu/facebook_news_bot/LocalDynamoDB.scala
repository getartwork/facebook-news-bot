package com.gu.facebook_news_bot

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBAsyncClient
import com.amazonaws.services.dynamodbv2.model._
import com.amazonaws.services.dynamodbv2.model.ScalarAttributeType._

import scala.collection.convert.decorateAsJava._

//Totally stolen from https://github.com/guardian/scanamo/blob/master/src/test/scala/com/gu/scanamo/LocalDynamoDB.scala
object LocalDynamoDB {

  def createUsersTable(name: String) = {
    println(s"Creating users table $name")
    tableWithSecondaryIndex(name, "notificationTimeUTC-ID-index")('ID -> S)('notificationTimeUTC -> S,'ID -> S)
  }

  def createUserTeamTable(name: String) = {
    println(s"Creating user-team table $name")
    tableWithSecondaryIndex(name, "team-ID-index")('ID -> S, 'team -> S)('team -> S, 'ID -> S)
  }

  def createUserNomsTable(name: String) = {
    println(s"Creating user-noms table $name")
    createTable(name)('ID -> S)
  }

  val client = {
    val c = new AmazonDynamoDBAsyncClient(new com.amazonaws.auth.BasicAWSCredentials("key", "secret"))
    c.setEndpoint("http://localhost:8000")
    c
  }

  def createTable(tableName: String)(attributes: (Symbol, ScalarAttributeType)*) = {
    client.createTable(
      attributeDefinitions(attributes),
      tableName,
      keySchema(attributes),
      arbitraryThroughputThatIsIgnoredByDynamoDBLocal
    )
  }

  def tableWithSecondaryIndex(tableName: String, secondaryIndexName: String)
                                 (primaryIndexAttributes: (Symbol, ScalarAttributeType)*)(secondaryIndexAttributes: (Symbol, ScalarAttributeType)*) = {
    client.createTable(
      new CreateTableRequest().withTableName(tableName)
        .withAttributeDefinitions(attributeDefinitions(
          primaryIndexAttributes.toList ++ (secondaryIndexAttributes.toList diff primaryIndexAttributes.toList)))
        .withKeySchema(keySchema(primaryIndexAttributes))
        .withProvisionedThroughput(arbitraryThroughputThatIsIgnoredByDynamoDBLocal)
        .withGlobalSecondaryIndexes(new GlobalSecondaryIndex()
          .withIndexName(secondaryIndexName)
          .withKeySchema(keySchema(secondaryIndexAttributes))
          .withProvisionedThroughput(arbitraryThroughputThatIsIgnoredByDynamoDBLocal)
          .withProjection(new Projection().withProjectionType(ProjectionType.ALL))
        )
    )
  }

  private def keySchema(attributes: Seq[(Symbol, ScalarAttributeType)]) = {
    val hashKeyWithType :: rangeKeyWithType = attributes.toList
    val keySchemas = hashKeyWithType._1 -> KeyType.HASH :: rangeKeyWithType.map(_._1 -> KeyType.RANGE)
    keySchemas.map{ case (symbol, keyType) => new KeySchemaElement(symbol.name, keyType)}.asJava
  }

  private def attributeDefinitions(attributes: Seq[(Symbol, ScalarAttributeType)]) = {
    attributes.map{ case (symbol, attributeType) => new AttributeDefinition(symbol.name, attributeType)}.asJava
  }

  private val arbitraryThroughputThatIsIgnoredByDynamoDBLocal = new ProvisionedThroughput(1L, 1L)
}
