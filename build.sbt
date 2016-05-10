name := "typed-dynamo"

version := "1.0"

scalaVersion := "2.11.8"

resolvers += "DynamoDB Local Release Repository" at "http://dynamodb-local.s3-website-us-west-2.amazonaws.com/release"

libraryDependencies ++= Seq(

  "com.github.seratch" %% "awscala" % "0.5.5",
  "com.amazonaws" % "DynamoDBLocal" % "1.10.5.1",

  "com.chuusai" %% "shapeless" % "2.3.0",

  "com.google.inject.extensions" % "guice-testlib" % "4.0" % "test" classifier "tests",
  "com.google.inject.extensions" % "guice-testlib" % "4.0" % "test"
)
