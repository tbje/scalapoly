import sbt._
import Keys._
import com.earldouglas._

name := "scalapoly"

organization := "groosker"

scalaVersion := Version.scala

// offline := true // Use when on the move...

resolvers ++= Seq (
        "spray" at "http://repo.spray.io/",
        "erisRepo" at "http://88.198.24.198/maven/", 
        "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

addCommandAlias("game", "runMain tbje.scalapoly.NormalGame")

addCommandAlias("webgame", "runMain tbje.scalapoly.WebGame")

addCommandAlias("restart", ";container:stop;container:start")

addCommandAlias("start", "container:start")

addCommandAlias("stop", "container:stop")

libraryDependencies ++= Dependencies.scalapoly

incOptions := incOptions.value.withNameHashing(true)

seq(webSettings :_*) 

scalacOptions ++= Seq(
//  "-unchecked",
  "-deprecation",
//  "-Xlint",
  "-language:_",
  "-target:jvm-1.6",
  "-encoding", "UTF-8"
)
