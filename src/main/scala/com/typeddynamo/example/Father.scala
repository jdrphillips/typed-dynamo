package com.typeddynamo
package example

case class Father(id: String, name: String, other: Option[Int] = None) extends DynamoEntity[String] {
  val hashPK = id
}
