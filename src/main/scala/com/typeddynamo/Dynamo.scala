package com.typeddynamo

import awscala.dynamodbv2.AttributeValue
import awscala.dynamodbv2.DynamoDBClient
import awscala.dynamodbv2.Item
import awscala.dynamodbv2.KeySchema
import awscala.dynamodbv2.ProvisionedThroughput
import awscala.dynamodbv2.Table
import awscala.dynamodbv2.TableMeta
import com.amazonaws.services.dynamodbv2.local.embedded.DynamoDBEmbedded
import com.amazonaws.services.{ dynamodbv2 => aws }
import scala.collection.JavaConverters._

// TODO remove the need for AWScala

trait Dynamo {

  def table(name: String): Option[Table]
  def listTables: Seq[String]
  def get(table: Table, hashPK: Any): Option[Item]
  def put(table: Table, hashPK: Any, attributes: (String, Any)*): Unit
  def createTable(table: Table): TableMeta
  def shutdown: Unit
  def deleteItem(table: Table, hashPK: Any): Unit

}

// We have to wrap our library's client because the embedded client is not extendable
class DynamoClient(client: DynamoDBClient) extends Dynamo {

  def table(name: String): Option[Table] = client.table(name)
  def listTables: Seq[String] = client.tableNames
  def get(table: Table, hashPK: Any): Option[Item] = client.get(table, hashPK)
  def put(table: Table, hashPK: Any, attributes: (String, Any)*): Unit = client.putItem(table, hashPK, attributes)
  def createTable(table: Table): TableMeta = client.createTable(table)
  def shutdown: Unit = client.shutdown
  def deleteItem(table: Table, hashPK: Any): Unit = client.deleteItem(table, hashPK)

}

// These are copied from awscala
class TestDynamoClient extends Dynamo {

  private lazy val client = DynamoDBEmbedded.create()

  def listTables: Seq[String] = client.listTables.getTableNames.asScala

  def table(name: String): Option[Table] = try {

    val tableMeta = Option(TableMeta(
      client.describeTable(new aws.model.DescribeTableRequest().withTableName(name)).getTable
    ))

    tableMeta.map(_.table)
  } catch { case e: aws.model.ResourceNotFoundException => None }

  def get(table: Table, hashPK: Any): Option[Item] = try {
    val attributes = client.getItem(new aws.model.GetItemRequest()
      .withTableName(table.name)
      .withKey(Map(table.hashPK -> AttributeValue.toJavaValue(hashPK)).asJava)
      .withConsistentRead(false)).getItem

    Option(attributes).map(Item(table, _))
  } catch { case e: aws.model.ResourceNotFoundException => None }

  private def attributeValues(attributes: Seq[(String, Any)]): java.util.Map[String, aws.model.AttributeValue] =
    attributes.toMap.mapValues(AttributeValue.toJavaValue(_)).asJava

  def put(table: Table, hashPK: Any, attributes: (String, Any)*): Unit = {
    val fullAttributes = Seq(table.hashPK -> hashPK) ++: attributes
    client.putItem(new aws.model.PutItemRequest()
      .withTableName(table.name)
      .withItem(attributeValues(fullAttributes)))
  }

  def createTable(table: Table): TableMeta = {
    val keySchema: Seq[aws.model.KeySchemaElement] = Seq(
      Some(KeySchema(table.hashPK, aws.model.KeyType.HASH)),
      table.rangePK.map(n => KeySchema(n, aws.model.KeyType.RANGE))
    ).flatten.map(_.asInstanceOf[aws.model.KeySchemaElement])

    val req = new aws.model.CreateTableRequest()
      .withTableName(table.name)
      .withAttributeDefinitions(table.attributes.map(_.asInstanceOf[aws.model.AttributeDefinition]).asJava)
      .withKeySchema(keySchema.asJava)
      .withProvisionedThroughput(
        table.provisionedThroughput.map(_.asInstanceOf[aws.model.ProvisionedThroughput]).getOrElse {
          ProvisionedThroughput(readCapacityUnits = 10, writeCapacityUnits = 10)
        }
      )

    if (!table.localSecondaryIndexes.isEmpty) req.setLocalSecondaryIndexes(
      table.localSecondaryIndexes.map(_.asInstanceOf[aws.model.LocalSecondaryIndex]).asJava
    )

    if (!table.globalSecondaryIndexes.isEmpty) req.setGlobalSecondaryIndexes(
      table.globalSecondaryIndexes.map(_.asInstanceOf[aws.model.GlobalSecondaryIndex]).asJava
    )

    TableMeta(client.createTable(req).getTableDescription)
  }

  def shutdown: Unit = client.shutdown

  def deleteItem(table: Table, hashPK: Any): Unit =
    client.deleteItem(new aws.model.DeleteItemRequest()
      .withTableName(table.name)
      .withKey(Map(table.hashPK -> AttributeValue.toJavaValue(hashPK)).asJava))

}
