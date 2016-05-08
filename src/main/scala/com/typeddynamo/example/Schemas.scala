package com.typeddynamo
package example

// This is where we write the table objects that Dynamo will understand
object Schemas extends TypeMappingImplicits {

  implicit object DynamoPerson extends DynamoTable[Person]("persons") {

    // TODO mark the hashpk for test-creation
    def Id   = column[String]("id")
    def Name = column[String]("name")
    def Age  = column[Age]("age")
    def Weight = column[Option[Int]]("weight")
    def ParentNames = column[Seq[String]]("parent_names")

    // TODO This doesn't have an exhaustion checks
    def toDynamoParams(t: Person): DynamoParams = {
      DynamoParams(Id.name, t.id) ~
        (Name.name, t.name) ~
        (Age.name, t.age) ~
        (Weight.name, t.weight) ~
        (ParentNames.name, t.parentNames)
    }

    def fromDynamo(params: DynamoParams): Person = {
      Person(
        params.notNull(Id),
        params.notNull(Name),
        params.notNull(Age),
        params.optionally(Weight),  // TODO: better way than 'notNull' and 'optionally'?
        params.notNull(ParentNames)
      )
    }

  }
}
