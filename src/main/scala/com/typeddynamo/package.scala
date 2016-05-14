package com

import awscala.dynamodbv2.AttributeValue
import shapeless.HList

package object typeddynamo {

  implicit class RichHlist[L <: HList](l: L) {
    def <>[E <: Entity, V <: HList](projection: E => V, extraction: V => E) = Proof(l)(projection, extraction)
  }

  implicit class RichBoolean(val b: Boolean) extends AnyVal {
    def toOption[T](ifTrue: T): Option[T] = if (b) Option(ifTrue) else Option.empty[T]
  }

  implicit class RichString(s: String) {

    def safeToDouble: Option[Double] = try { Option(s.toDouble) } catch {
      case n: NumberFormatException => None
    }

    def safeToInt: Option[Int] = try { Option(s.toInt) } catch {
      case n: NumberFormatException => None
    }

  }

  implicit class RichAttributeValue(av: AttributeValue) {
    def anyValue: Option[Any] = {
      val ss = av.ss.nonEmpty.toOption(av.ss)
      val bs = av.ss.nonEmpty.toOption(av.bs)

      def ints: Option[Seq[Int]] = {
        if (av.ns.exists(_.safeToInt.isEmpty)) None
        else Some(av.ns.flatMap(_.safeToInt))
      }

      def doubles: Option[Seq[Double]] = {
        val ds = av.ns.flatMap(_.safeToDouble)
        ds.nonEmpty.toOption(ds)
      }

      def double(s: String): Option[Double] = s.safeToDouble
      def int(s: String): Option[Int] = double(s).flatMap(d => (d.toInt == d).toOption(d.toInt))

      av.s orElse
        av.n.flatMap(int) orElse
        av.n.flatMap(double) orElse
        av.b orElse
        av.m orElse
        ss orElse
        ints orElse
        doubles orElse
        bs
    }
  }

  type Entity = DynamoEntity[Any]

}
