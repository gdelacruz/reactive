name := "reactive" 

version := "1.0.0"

scalaVersion := "2.10.2"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.2.1"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.2.1"
