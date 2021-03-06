package com.typeddynamo

import collection.JavaConverters._

import shapeless.::
import shapeless.HList
import shapeless.HNil
import shapeless.ops.hlist.RightFolder
import shapeless.ops.hlist.Zip

import DynamoValue._
import ShapeUtils._

abstract class QueryObject[
  T,
  E <: DynamoEntity[T],
  Values <: HList,
  Columns <: HList,
  Z <: HList
](implicit
  val zipper: Zip.Aux[Columns :: Values :: HNil, Z],
  val folderCollapse: RightFolder.Aux[Z, DynamoParams, CollapseColumnAndValuesToParams.type, DynamoParams],
  val folderExtract: RightFolder.Aux[Columns, (HNil, DynamoParams), ExtractColumnValuesFromParams.type, (Values, DynamoParams)]
) {

  def table: DynamoTable[T, E]

  def proof: Proof[E, Values, Columns]

  def toDynamoParams(e: E): DynamoParams = project(proof.columns, proof.projection(e))

  def fromDynamo(params: DynamoParams): E = proof.extraction(extract(proof.columns, params))

  protected def basicValueFromRawDynamo(value: Any): Option[DynamoBasicType[Any]] = {
    value match {
      case i: Int => Some(DynamoInt(i))
      case d: Double => Some(DynamoDouble(d))
      case s: String => Some(DynamoString(s))
      case b: Boolean => Some(DynamoBoolean(b))
      case null | None => None
      case x => throw new Exception(s"The type of $x is not a basic dynamo supported type")
    }
  }

  protected def valueFromRawDynamo(value: Any): Option[DynamoValue[Any]] = {
    value match {
      case x @ (Int | Double | Boolean) => basicValueFromRawDynamo(value)
      case d: java.lang.Double =>          basicValueFromRawDynamo(d.doubleValue)
      case d: java.lang.Integer =>         basicValueFromRawDynamo(d.intValue)
      case s: String =>                    basicValueFromRawDynamo(s)
      case s: Traversable[Any] =>          Some(DynamoSeq(s.toSeq.flatMap(basicValueFromRawDynamo)))
      case null | None => None
      case x => throw new Exception(s"The type of $x: ${x.getClass} is not yet supported in our dynamo implementation")
    }
  }

  def toRawDynamoParams(t: E): Seq[(String, Any)] = toDynamoParams(t).params.toSeq.flatMap { case (name, value) =>

    def rawValue(v: Option[DynamoValue[Any]]): Option[Any] = v match {
      case Some(DynamoBoolean(b)) => Some(b)
      case Some(DynamoInt(i)) => Some(i)
      case Some(DynamoDouble(d)) => Some(d)
      case Some(DynamoString(s)) => Some(s)
      case Some(DynamoSeq(s)) => s match {
        case Nil => None
        case other => Option(other.map(Option.apply).flatMap(rawValue))
      }
      case Some(DynamoNull) | None | null => None
    }

    // We elide empty values: Dynamo cannot handle writing them
    rawValue(value).map(name -> _)
  }

  def fromRawDynamo(params: Seq[(String, Option[Any])]): E = {
    val typesafe = params.map { case (name, value) =>
      name -> Option(value).flatten.flatMap(valueFromRawDynamo)
    }
    fromDynamo(DynamoParams(typesafe.toMap))
  }
}

object QueryObject {
  def apply[
    T,
    E <: DynamoEntity[T],
    Values <: HList,
    Columns <: HList,
    Z <: HList
  ](
    tbl: DynamoTable[T, E]
  )(
    prf: Proof[E, Values, Columns]
  )(implicit
    zipper: Zip.Aux[Columns :: Values :: HNil, Z],
    folderCollapse: RightFolder.Aux[Z, DynamoParams, CollapseColumnAndValuesToParams.type, DynamoParams],
    folderExtract: RightFolder.Aux[Columns, (HNil, DynamoParams), ExtractColumnValuesFromParams.type, (Values, DynamoParams)]
  ) = new QueryObject[T, E, Values, Columns, Z] {
    def table = tbl
    def proof = prf
  }
}
