package com.typeddynamo

import shapeless.::
import shapeless.Poly2
import shapeless.HList
import shapeless.HNil
import shapeless.ops.hlist.RightFolder
import shapeless.ops.hlist.Zip

trait ShapeUtils {

  object ExtractColumnValuesFromParams extends Poly2 {
    implicit def extract[T, L <: HList]: Case.Aux[
      Column[T],
      (L, DynamoParams),
      (T :: L, DynamoParams)
    ] = at[Column[T], (L, DynamoParams)] {
      case (t, (acc, params)) => (params.get(t) :: acc, params)
    }
  }

  object CollapseColumnAndValuesToParams extends Poly2 {
    implicit def collapse[T, L <: HList]: Case.Aux[
      (Column[T], T),
      DynamoParams,
      DynamoParams
    ] = at[(Column[T], T), DynamoParams] {
      case ((col, value), params) => params ~ (col, value)
    }
  }

  def project[C <: HList, V <: HList, Z <: HList](
    columns: C,
    values: V
  )(implicit
    zipper: Zip.Aux[C :: V :: HNil, Z],
    folder: RightFolder.Aux[Z, DynamoParams, CollapseColumnAndValuesToParams.type, DynamoParams]
  ): DynamoParams = {
    columns.zip(values).foldRight(DynamoParams.empty)(CollapseColumnAndValuesToParams)
  }

  def extract[A <: HList, L <: HList](hlist: A, params: DynamoParams)(implicit r: RightFolder.Aux[
    A,
    (HNil, DynamoParams),
    ExtractColumnValuesFromParams.type,
    (L, DynamoParams)
  ]): L = {
    hlist.foldRight((HNil: HNil, params))(ExtractColumnValuesFromParams)._1
  }

}
