package com.typeddynamo

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.language.higherKinds

import scalaz._
import scalaz.syntax._
import scalaz.syntax.std._
import scalaz.std._

object Scalaz {

  trait Predef extends Instances

  trait Syntax extends ToApplicativeOps with ToBooleanOps with ToMonadOps with ToTraverseOps with ToOptionOps
    with ToFoldable1Ops with ToIdOps with ToMonoidOps

  trait Instances
    extends Syntax
    with FunctionInstances
    with ListInstances
    with SetInstances
    with OptionInstances
    with FutureInstances
    with StringInstances
    with AnyValInstances
    with TupleInstances
  {
    implicit val seqInstance = new Traverse[Seq] with MonadPlus[Seq] {
      def point[A](a: => A): Seq[A] = Seq(a)
      def bind[A, B](fa: Seq[A])(f: A => Seq[B]): Seq[B] = fa.flatMap(f)
      def plus[A](a: Seq[A],b: => Seq[A]): Seq[A] = a ++ b
      def empty[A]: Seq[A] = Nil

      def traverseImpl[F[_], A, B](xs: Seq[A])(f: A => F[B])(implicit F: Applicative[F]): F[Seq[B]] =
        xs.toList.traverse(f).map(_.toSeq)
    }

    implicit def seqMonoid[A]: Monoid[Seq[A]] = new Monoid[Seq[A]] {
      def append(f1: Seq[A], f2: => Seq[A]) = f1 ++ f2
      def zero: Seq[A] = Vector.empty
    }
  }

}
