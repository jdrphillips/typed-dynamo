package com.typeddynamo

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

  protected def valueFromRawDynamo(value: Any): Option[DynamoValue[Any]] = {
    value match {
      case i: Int => Some(DynamoInt(i))
      case s: String => Some(DynamoString(s))
      case b: Boolean => Some(DynamoBoolean(b))
      case s: Seq[Any] => Some(DynamoSeq(s.flatMap(valueFromRawDynamo)))
      case null | None => None
      case x => throw new Exception(s"The type of $x is not yet supported in our dynamo implementation")
    }
  }

  def toRawDynamoParams(t: E): Seq[(String, Any)] = toDynamoParams(t).params.toSeq.flatMap { case (name, value) =>
    val rawValue = value match {
      case Some(DynamoBoolean(b)) => Some(b)
      case Some(DynamoInt(i)) => Some(i)
      case Some(DynamoString(s)) => Some(s)
      case Some(DynamoSeq(s)) => s match {
        case Nil => None
        case other => Some(other)
      }
      case Some(DynamoNull) | None | null => None
    }
    // We elide empty values: Dynamo cannot handle writing them
    rawValue.map(name -> _)
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
