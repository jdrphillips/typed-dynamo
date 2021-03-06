package com.typeddynamo
package example

import shapeless.HNil
import shapeless.Generic
import shapeless.::
import shapeless.nat._

// This is where we write the table objects that Dynamo will understand
object Schemas extends TypeMappingImplicits {

  // A boring, ordinary example showing the minimum code needed.
  // Examples of all interesting column types are included
  implicit object DynamoPerson extends DynamoTable[String, Person]("persons") {
    // These are your columns in the db.
    def Id = column[String]("id")
    def Name = column[String]("name")
    def Age = column[Age]("age")
    def Weight = column[Option[Int]]("weight")
    def ParentNames = column[Seq[String]]("parent_names")

    // This has to match your dynamo table definition or nothing will work correctly!
    def hashPK = Id

    val columns = Id :: Name :: Age :: Weight :: ParentNames :: HNil
  }

  val dynamoPerson = {
    import DynamoPerson._
    // This is to tell shapeless how to turn a case class into an HList. You don't need it if
    // you want to create your mappings by hand.
    val PersonGen = Generic[Person]
    QueryObject(DynamoPerson) {
      // This is the shape of your table.
      // It's not necessary for it to be the same as 'Person', but this approach allows more automation.
      columns <> (PersonGen.to, PersonGen.from)
    }
  }

  // An example where we mutate data before writing in to the db, and after reading out.
  // The mutation can be arbitrary
  implicit object DynamoFather extends DynamoTable[String, Father]("fathers") {
    def Id = column[String]("id")
    def Name = column[String]("name")

    def hashPK = Id

  }

  val dynamoFather = {
    import DynamoFather._
    QueryObject(DynamoFather) {
      Id :: Name :: HNil <> (
        { f: Father => f.id :: f.name.toUpperCase :: HNil },
        // For some reason I haven't figured out, you cannot decompose an hlist when the argument to this function
        // So if you want to mutate your data on the way in and out, you'll you need to extract it from the hlist
        // by hand with more boilerplate
        { hlist: (String :: String :: HNil) =>
          Father(
            id = hlist(_0),
            name = hlist(_1).toLowerCase,
            None
          )
        }
      )
    }
  }

}
