package com.typeddynamo

abstract class DynamoTable[E <: DynamoEntity](val name: String) {

  protected def column[U](n: String)(implicit ev: TypeMapper[U]): Column[U] = new Column[U] {
    val name = n
    def evidence = ev
  }

  def rawTable(implicit dynamoDb: Dynamo): awscala.dynamodbv2.Table =
    dynamoDb.table(name).get  // TODO proper error & future

}
