name := "futuresBlog"

version := "1.0"

scalaVersion := "2.11.7"

version := "0.1.0"

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

javaOptions ++= Seq("-Xmx4G", "-XX:+UseConcMarkSweepGC")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"