package com.typeddynamo

sealed trait DynamoValue[+T] { def value: T }
sealed trait DynamoBasicType[+T] extends DynamoValue[T]
sealed trait DynamoCollectionType[+T] extends DynamoValue[T]

object DynamoValue {

  // Natively supported types
  case class DynamoInt(value: Int) extends DynamoBasicType[Int]
  case class DynamoDouble(value: Double) extends DynamoBasicType[Double]
  case class DynamoString(value: String) extends DynamoBasicType[String]

  // Boolean's serialized value is an int since our wrapper library can't read booleans
  case class DynamoBoolean(value: Int) extends DynamoBasicType[Int]
  object DynamoBoolean {
    def apply(b: Boolean): DynamoBoolean = DynamoBoolean(if (b) 1 else 0)
  }

  case object DynamoNull extends DynamoBasicType[Nothing] {
    def value = throw new Exception("Tried to extract a value from a null field in DynamoDB")
  }

  case class DynamoSeq(value: Seq[DynamoBasicType[Any]]) extends DynamoCollectionType[Seq[DynamoBasicType[Any]]]

}
