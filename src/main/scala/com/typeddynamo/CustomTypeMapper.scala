package com.typeddynamo

trait CustomTypeMapper[T] extends BasicTypeMapper[T] {
  type Primitive
  implicit def primitiveEvidence: DynamoPrimitiveTypeMapper[Primitive]

  def _serialize(t: T): Primitive
  def _deserialize(u: Primitive): T

  def serialize(t: T): DynamoBasicType[Any] = primitiveEvidence.serialize(_serialize(t))
  def deserialize(u: DynamoValue[Any]): T = _deserialize(primitiveEvidence.deserialize(u))
}

object CustomTypeMapper {

  def apply[T, U](
    to: T => U,
    from: U => T
  )(implicit primitive: DynamoPrimitiveTypeMapper[U]): CustomTypeMapper[T] = {
    new CustomTypeMapper[T] {
      type Primitive = U
      implicit def primitiveEvidence = primitive
      def _serialize(t: T): Primitive = to(t)
      def _deserialize(u: Primitive): T = from(u)
    }
  }

}
