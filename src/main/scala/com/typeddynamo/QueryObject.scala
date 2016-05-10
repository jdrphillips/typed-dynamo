package com.typeddynamo

import shapeless.::
import shapeless.HList
import shapeless.HNil
import shapeless.ops.hlist.RightFolder
import shapeless.ops.hlist.Zip

import DynamoValue._

trait QueryObject[E <: DynamoEntity, Values <: HList, Columns <: HList] extends ShapeUtils {

  def table: DynamoTable[E]

  def proof: Proof[E, Values, Columns]

  def toDynamoParams[Z <: HList](e: E)(implicit
    zipper: Zip.Aux[Columns :: Values :: HNil, Z],
    folder: RightFolder.Aux[Z, DynamoParams, CollapseColumnAndValuesToParams.type, DynamoParams]
  ): DynamoParams = project(proof.columns, proof.projection(e))

  def fromDynamo[Z <: HList](params: DynamoParams)(implicit r: RightFolder.Aux[
    Columns,
    (HNil, DynamoParams),
    ExtractColumnValuesFromParams.type,
    (Values, DynamoParams)
  ]): E = proof.extraction(extract(proof.columns, params))

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

  def toRawDynamoParams[Z <: HList](t: E)(implicit
    zipper: Zip.Aux[Columns :: Values :: HNil, Z],
    folder: RightFolder.Aux[Z, DynamoParams, CollapseColumnAndValuesToParams.type, DynamoParams]
  ): Seq[(String, Any)] = toDynamoParams(t).params.toSeq.flatMap { case (name, value) =>
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

  def fromRawDynamo[Z <: HList](params: Seq[(String, Option[Any])])(implicit r: RightFolder.Aux[
    Columns,
    (HNil, DynamoParams),
    ExtractColumnValuesFromParams.type,
    (Values, DynamoParams)
  ]): E = {
    val typesafe = params.map { case (name, value) =>
      name -> Option(value).flatten.flatMap(valueFromRawDynamo)
    }
    fromDynamo(DynamoParams(typesafe.toMap))
  }
}

import shapeless.::
import shapeless.HList
import shapeless.HNil
import shapeless.ops.hlist.RightFolder
import shapeless.ops.hlist.Zip

import DynamoValue._

object QueryObject {
  def apply[
    E <: DynamoEntity,
    Values <: HList,
    Columns <: HList
  ](tbl: DynamoTable[E])(prf: Proof[E, Values, Columns]) = new QueryObject[E, Values, Columns] {
    def table = tbl
    def proof = prf
  }
}
