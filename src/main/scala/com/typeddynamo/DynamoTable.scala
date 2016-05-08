package com.typeddynamo

import DynamoValue._

trait Column[T] { def name: String }

sealed trait Transformer[T <: DynamoEntity] {

  protected def valueFromRawDynamo(value: Any): Option[DynamoValue[Any]] = {
    value match {
      case i: Int => Some(DynamoInt(i))
      case s: String => Some(DynamoString(s))
      case b: Boolean => Some(DynamoBoolean(b))
      case s: Seq[Any] => Some(DynamoSeq(s.flatMap(valueFromRawDynamo)))
      case null | None => None
      case x => throw new Exception(s"The type of $x is not yet supported in our dynamo implementation")
    }
  }

  def toRawDynamoParams(t: T): Seq[(String, Any)] = toDynamoParams(t).params.toSeq.flatMap { case (name, value) =>
    val rawValue = value match {
      case Some(DynamoBoolean(b)) => Some(b)
      case Some(DynamoInt(i)) => Some(i)
      case Some(DynamoString(s)) => Some(s)
      case Some(DynamoSeq(s)) => s match {
        case Nil => None
        case other => Some(other)
      }
      case Some(DynamoNull) | None | null => None
    }
    // We elide empty values: Dynamo cannot handle writing them
    rawValue.map(name -> _)
  }

  def toDynamoParams(t: T): DynamoParams

  def fromDynamo(m: DynamoParams): T

  def fromRawDynamo(params: Seq[(String, Option[Any])]): T = {
    val typesafe = params.map { case (name, value) =>
      name -> Option(value).flatten.flatMap(valueFromRawDynamo)
    }

    fromDynamo(DynamoParams(typesafe.toMap))
  }

}

trait DynamoTable[T <: DynamoEntity] extends Transformer[T] {

  def name: String

  protected def column[U](n: String) = new Column[U] { val name = n }

  def rawTable(implicit dynamoDb: Dynamo): awscala.dynamodbv2.Table =
    dynamoDb.table(name).get  // TODO proper error

}
