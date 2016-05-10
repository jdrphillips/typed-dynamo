package com.typeddynamo
package example

// In your project, create one of these traits and write your custom type mappers here
trait TypeMappingImplicits extends PrimitiveTypeMappers {

  // For a custom type you want to store, you need to say how to turn it into a Dynamo supported
  // type.
  implicit lazy val ageTypeMapper = CustomTypeMapper[Age, Int](
    { a: Age => a.value },
    { i: Int => Age(i) }
  )

  // This is safe. You can't give a type you're not allowed to on the right. The following would not compile:
  // implicit lazy val ageTypeMapper = CustomTypeMapper[Age, Age](
  //   { a: Age => a },
  //   { a: Age => a }
  // )
}
