package com.typeddynamo

import shapeless.HList

sealed trait Proof[E <: DynamoEntity, Values <: HList, Columns <: HList] {
  def columns: Columns
  def projection: E => Values
  def extraction: Values => E
}

object Proof {
  def apply[E <: DynamoEntity, Values <: HList, Columns <: HList](
    col: Columns
  )(pro: E => Values, ext: Values => E): Proof[E, Values, Columns] = new Proof[E, Values, Columns] {
    val columns = col
    val projection = pro
    val extraction = ext
  }
}
