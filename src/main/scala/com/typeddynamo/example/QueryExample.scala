package com.typeddynamo
package example

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future

import Implicits._
import Operation.{read, insert}

object QueryExample {

  val db: Dynamo = ???  // Inject your DB instance here

  val query = {
    for {
      j <- read(Schemas.dynamoPerson, "john")
      f <- read(Schemas.dynamoFather, j.name)
      newFather = Father(f.name.toUpperCase, f.name)
      _ <- insert(Schemas.dynamoFather, newFather)
    } yield newFather
  }

  val result: Future[Father] = query.execute(db)

}
