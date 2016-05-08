package com.typeddynamo
package example

// This is a case class we will actually store in and retrieve from Dynamo
case class Person(
  id: String,
  name: String,
  age: Age,
  weight: Option[Int],
  middleName: Option[String],
  parentNames: Seq[String]
) extends DynamoEntity
