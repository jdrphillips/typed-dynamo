package com.typeddynamo

case class DynamoParams(params: Map[String, Option[DynamoValue[Any]]]) {

  def get[U](column: Column[U]): U = {
    val x = params.get(column.name).flatten
    column.evidence.optionDeserialize(x)
  }

  def ~[T](thatColumn: Column[T], thatValue: T): DynamoParams =
    DynamoParams(this.params + ((thatColumn.name, Some(thatColumn.evidence.serialize(thatValue)))))
  def ~[T](thatColumn: Column[Option[T]], thatValue: Option[T]): DynamoParams =
    DynamoParams(this.params + ((thatColumn.name, Some(thatColumn.evidence.serialize(thatValue)))))
}

// TODO Monoid
object DynamoParams {

  val empty = DynamoParams(Map.empty)

  def apply[T](column: Column[T], value: T): DynamoParams =
    DynamoParams(Map(column.name -> Some(column.evidence.serialize(value))))

  def apply[T](column: Column[Option[T]], value: Option[T]): DynamoParams =
    DynamoParams(Map(column.name -> Some(column.evidence.serialize(value))))

}
