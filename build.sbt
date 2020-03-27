scalaVersion := "2.13.1"
version := "0.1.0-SNAPSHOT"
organization := "com.kentfrazier"
name := "purely-functional-data-structures-scala"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.14.1",
  "org.scalatest" %% "scalatest" % "3.1.1",
  "org.scalatestplus" %% "scalacheck-1-14" % "3.1.1.1"
).map(_ % Test)