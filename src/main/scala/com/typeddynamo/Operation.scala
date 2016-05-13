package com.typeddynamo

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import scalaz.~>
import scalaz.Free
import shapeless.HList

import Implicits._
import ShapeUtils._

sealed trait Operation[T]

object Operation {

  case class Read[T, E <: DynamoEntity[T], V <: HList, C <: HList, Z <: HList](obj: QueryObject[T, E, V, C, Z], id: String) extends Operation[E]
  case class Insert[T, E <: DynamoEntity[T], V <: HList, C <: HList, Z <: HList](obj: QueryObject[T, E, V, C, Z], entity: E) extends Operation[Unit]
  case class Delete[T, E <: DynamoEntity[T], V <: HList, C <: HList, Z <: HList](obj: QueryObject[T, E, V, C, Z], id: String) extends Operation[Unit]

  type FreeOperation[X] = Free[Operation, X]

  def read[T, E <: DynamoEntity[T], V <: HList, C <: HList, Z <: HList](obj: QueryObject[T, E, V, C, Z], id: String): FreeOperation[E] =
    Free.liftF(Read(obj, id))

  def insert[T, E <: DynamoEntity[T], V <: HList, C <: HList, Z <: HList](obj: QueryObject[T, E, V, C, Z], entity: E): FreeOperation[Unit] =
    Free.liftF(Insert(obj, entity))

  def delete[T, E <: DynamoEntity[T], V <: HList, C <: HList, Z <: HList](obj: QueryObject[T, E, V, C, Z], id: String): FreeOperation[Unit] =
    Free.liftF(Delete(obj, id))

  def DynamoInvoker(db: Dynamo) = new (Operation ~> Future) {
    override def apply[A](e: Operation[A]): Future[A] = e match {

      case Read(obj, id) => Future {
        val table = obj.table.rawTable(db)
        val item = db.get(table, id).getOrElse(
          throw new DynamoException(s"No entity with id $id found in table ${obj.table.name}")
        )
        val params = item.attributes.map { a => a.name -> a.value.anyValue }
        obj.fromRawDynamo(params)
      }

      case Insert(obj, entity) => Future {
        val table = obj.table.rawTable(db)
        val params = obj.toRawDynamoParams(entity)
        db.put(table, entity.hashPK, params: _*)
      }

      case Delete(obj, id) => Future {
        val table = obj.table.rawTable(db)
        db.deleteItem(table, id)
      }

    }
  }
}
