package com.typeddynamo

import DynamoValue._

trait TypeMapper[T] {
  def serialize(t: T): DynamoValue[Any]
  def deserialize(u: DynamoValue[Any]): T
}

sealed trait DynamoPrimitiveTypeMapper[T] extends TypeMapper[T] {
  def serialize(t: T): DynamoValue[Any]
  def deserialize(v: DynamoValue[Any]): T
}

trait PrimitiveTypeMappers {

  implicit object DynamoIntType extends DynamoPrimitiveTypeMapper[Int] {
    def serialize(i: Int): DynamoInt = DynamoInt(i)
    def deserialize(v: DynamoValue[Any]): Int = v match {
      case i: DynamoInt => i.value
      case x => throw new Exception(s"Expected Int from dynamodb column, received $x")
    }
  }

  implicit object DynamoStringType extends DynamoPrimitiveTypeMapper[String] {
    def serialize(s: String): DynamoString = DynamoString(s)
    def deserialize(v: DynamoValue[Any]): String = v match {
      case s: DynamoString => s.value
      case x => throw new Exception(s"Expected String from dynamodb column, received $x")
    }
  }

  implicit object DynamoBooleanType extends DynamoPrimitiveTypeMapper[Boolean] {
    def serialize(s: Boolean): DynamoBoolean = DynamoBoolean(if (s) 1 else 0)
    def deserialize(v: DynamoValue[Any]): Boolean = v match {
      case b: DynamoBoolean => b.value == 1
      case x => throw new Exception(s"Expected Boolean from dynamodb column, received $x")
    }
  }

  implicit object DynamoNullType extends DynamoPrimitiveTypeMapper[Nothing] {
    def serialize(s: Nothing): DynamoNull.type = DynamoNull
    def deserialize(v: DynamoValue[Any]): Nothing =
      throw new Exception(s"Attempting to deserialize a null value from DynamoDb")
  }

  implicit def optionMapper[T](implicit evidence: TypeMapper[T]) = new TypeMapper[Option[T]] {
    def serialize(t: Option[T]): DynamoValue[Any] = t.map(evidence.serialize).getOrElse(DynamoNull)
    def deserialize(d: DynamoValue[Any]): Option[T] = d match {
      case DynamoNull => None
      case other => Some(evidence.deserialize(other))
    }
  }

  implicit def seqMapper[T](implicit evidence: TypeMapper[T]) = new TypeMapper[Seq[T]] {

    def serialize(t: Seq[T]): DynamoValue[Any] = t match {
      case Seq() => DynamoNull
      case x => DynamoSeq(x.map(evidence.serialize))
    }

    def deserialize(d: DynamoValue[Any]): Seq[T] = d match {
      case DynamoNull => Seq()
      case DynamoSeq(s) => s.map(evidence.deserialize)
      case other => throw new Exception(s"Expected seq from Dynamo, got $other")
    }

  }
}
