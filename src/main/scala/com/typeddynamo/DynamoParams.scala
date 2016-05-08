package com.typeddynamo

case class DynamoParams(params: Map[String, Option[DynamoValue[Any]]]) {

  private def get[T <: DynamoEntity, U](column: Column[U])(implicit table: DynamoTable[T]): Option[DynamoValue[Any]] = {
    val x: Option[DynamoValue[Any]] = params
      .get(column.name)
      // TODO: Will this cause a problem? Requires testing.
      .getOrElse(throw new Exception(s"No value for column ${column.name} was found in dynamo for table ${table.name}"))
    x
  }

  def optionally[T <: DynamoEntity, U](
    column: Column[Option[U]]
  )(implicit table: DynamoTable[T], mapper: TypeMapper[Option[U]]): Option[U] = {
    val x = get[T, Option[U]](column)
    x.flatMap(mapper.deserialize)
  }

  def notNull[T <: DynamoEntity, U](column: Column[U])(implicit table: DynamoTable[T], mapper: TypeMapper[U]): U = {
    val x = get[T, U](column)
    val any = x.getOrElse(throw new Exception(s"Null value found in column ${column.name} for table ${table.name}"))
    mapper.deserialize(any)
  }

  def ~[T](thatKey: String, thatValue: T)(implicit mapper: TypeMapper[T]): DynamoParams =
    DynamoParams(this.params + ((thatKey, Some(mapper.serialize(thatValue)))))
  def ~[T](thatKey: String, thatValue: Option[T])(implicit mapper: TypeMapper[Option[T]]): DynamoParams =
    DynamoParams(this.params + ((thatKey, Some(mapper.serialize(thatValue)))))
}

object DynamoParams {
  // TODO allow column?

  def apply[T](key: String, value: T)(implicit mapper: TypeMapper[T]): DynamoParams =
    DynamoParams(Map(key -> Some(mapper.serialize(value))))

  def apply[T](key: String, value: Option[T])(implicit mapper: TypeMapper[Option[T]]): DynamoParams =
    DynamoParams(Map(key -> Some(mapper.serialize(value))))

}
