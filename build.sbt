name := "Garden"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= 
  Seq( "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
       "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
       "org.scala-lang" % "scala-compiler" % scalaVersion.value,
       "org.scalautils" % "scalautils_2.11" % "2.1.5" )
