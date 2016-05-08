package com.typeddynamo
package example

// In your project, create one of these traits and write your custom type mappers here
trait TypeMappingImplicits extends PrimitiveTypeMappers {
  implicit lazy val ageTypeMapper = CustomTypeMapper[Age, Int](
    { a: Age => a.value },
    { i: Int => Age(i) }
  )
}
