name := "Basic Algorithms"

version := "1.0"

scalaVersion := "2.11.4"

resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases"

libraryDependencies ++= Seq {
	val scalatestV = "2.2.1"
  "org.scalatest" %% "scalatest" % scalatestV % "test"
}
