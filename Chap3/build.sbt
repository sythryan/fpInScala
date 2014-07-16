name := "Chap3"
   
version := "1.0"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.2.2" % "test"
)

parallelExecution in Test := false
