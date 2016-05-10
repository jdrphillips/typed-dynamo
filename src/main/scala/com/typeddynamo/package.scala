package com

import shapeless.HList

package object typeddynamo {
  implicit class RichHlist[L <: HList](l: L) {
    def <>[E <: DynamoEntity, V <: HList](projection: E => V, extraction: V => E) = Proof(l)(projection, extraction)
  }
}
