package com.typeddynamo
package example

case class Father(id: String, name: String) extends DynamoEntity[String] {
  val hashPK = id
}
