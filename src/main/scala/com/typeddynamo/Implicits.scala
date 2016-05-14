package com.typeddynamo

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

trait Implicits extends Scalaz.Predef {

  implicit class RichFreeDynamoOperation[E](free: Operation.FreeOperation[E]) {
    def execute(db: Dynamo)(implicit ec: ExecutionContext): Future[E] = free.foldMap(Operation.DynamoInvoker(db))
  }

}

object Implicits extends Implicits
