package com.typeddynamo

sealed trait DynamoValue[+T] { def value: T }
object DynamoValue {

  // Natively supported types
  case class DynamoInt(value: Int) extends DynamoValue[Int]
  case class DynamoString(value: String) extends DynamoValue[String]

  // Boolean's serialized value is an int since our wrapper library can't read booleans
  case class DynamoBoolean(value: Int) extends DynamoValue[Int]
  object DynamoBoolean {
    def apply(b: Boolean): DynamoBoolean = DynamoBoolean(if (b) 1 else 0)
  }

  case object DynamoNull extends DynamoValue[Nothing] {
    def value = throw new Exception("Tried to extract a value from a null field in DynamoDB")
  }

  case class DynamoSeq(value: Seq[DynamoValue[Any]]) extends DynamoValue[Seq[DynamoValue[Any]]]

  // TODO DynamoMap

}
