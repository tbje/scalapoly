import sbt._

object Version {
  val akka             = "2.2.3"
  val facelift         = "0.1-SNAPSHOT"
  val lift             = "2.5"
  val jetty            = "9.1.3.v20140225"
  val scala            = "2.10.4"
  val logback          = "1.0.13"
  val dispatch         = "0.11.0"
  val slf4jLog4j12     = "1.7.5"
  val specs2           = "1.14"
  val junit            = "4.7"
  val sprayJson        = "1.2.5"
}

object Library {
  val facelift         = "tbje"                    %% "facelift"             % Version.facelift changing()
  val liftwebWebkit    = "net.liftweb"             %% "lift-webkit"          % Version.lift 
  val liftwebJson      = "net.liftweb"             %% "lift-json-ext"        % Version.lift
  val dispatch         = "net.databinder.dispatch" %% "dispatch-core"        % Version.dispatch
  val slf4jLog4j12     = "org.slf4j"               %  "slf4j-log4j12"        % Version.slf4jLog4j12
  val specs2           = "org.specs2"              %% "specs2"               % Version.specs2
  val junit            = "junit"                   %  "junit"                % Version.junit
  val jettyWebapp      = "org.eclipse.jetty"       %  "jetty-webapp"         % Version.jetty
  val jettyServer      = "org.eclipse.jetty"       %  "jetty-server"         % Version.jetty
  val logbackClassic   = "ch.qos.logback"          %  "logback-classic"      % Version.logback
  val akkaActor        = "com.typesafe.akka"       %% "akka-actor"           % Version.akka
  val akkaSlf4j        = "com.typesafe.akka"       %% "akka-slf4j"           % Version.akka
  val sprayJson        = "io.spray"                %% "spray-json"           % Version.sprayJson
}

object Dependencies {

  import Library._

  val scalapolyTest = List(
    junit,
    specs2
  ).map(_ % "test")

  val scalapolyContainer = List(
    jettyWebapp,
    jettyServer
  ) map { _ % "container" }

  val scalapoly = List(
   akkaActor, 
   akkaSlf4j,
   sprayJson,
   facelift,
   liftwebWebkit,
   liftwebJson,
   dispatch,
   slf4jLog4j12
  ) ++ scalapolyTest ++ scalapolyContainer
}
