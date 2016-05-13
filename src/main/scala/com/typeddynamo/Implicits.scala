package com.typeddynamo

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import awscala.dynamodbv2.AttributeValue

trait Implicits extends Scalaz.Predef {

  implicit class RichFreeDynamoOperation[E](free: Operation.FreeOperation[E]) {
    def execute(db: Dynamo)(implicit ec: ExecutionContext): Future[E] = free.foldMap(Operation.DynamoInvoker(db))
  }

  implicit class RichAttributeValue(av: AttributeValue) {
    def anyValue: Option[Any] = {
      val ss = av.ss.nonEmpty.toOption(av.ss)
      val ns = av.ss.nonEmpty.toOption(av.ns)
      val bs = av.ss.nonEmpty.toOption(av.bs)
      (av.s orElse av.n orElse av.b orElse av.m orElse ss orElse ns orElse bs)
    }
  }
}

object Implicits extends Implicits
