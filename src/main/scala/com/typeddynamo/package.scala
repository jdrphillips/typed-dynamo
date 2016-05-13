package com

import shapeless.HList

package object typeddynamo {

  implicit class RichHlist[L <: HList](l: L) {
    def <>[E <: Entity, V <: HList](projection: E => V, extraction: V => E) = Proof(l)(projection, extraction)
  }

  implicit class RichBoolean(val b: Boolean) extends AnyVal {
    def toOption[T](ifTrue: T): Option[T] = if (b) Option(ifTrue) else Option.empty[T]
  }

  type Entity = DynamoEntity[Any]

}
