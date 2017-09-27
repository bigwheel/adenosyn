//--------------------------------//
//       Shared Settings          //
//--------------------------------//

val waitMiddlewareBoot = taskKey[Unit](
  "boot middlewares and wait warms up if no runs"
)

waitMiddlewareBoot in Global := {
  import scala.sys.process.Process
  if (Process("docker-compose ps -q").!!.length == 0) {
    Process("docker-compose up -d").!
    Thread.sleep(10 * 1000L)
  }
}

//test in Test := (test in Test).dependsOn(waitMiddlewareBoot).value

lazy val baseSettings = Seq(
  organization := "com.github.bigwheel",
  version := "1.0.0",
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
  test in assembly := {},
  test in Test := (test in Test).dependsOn(waitMiddlewareBoot in Global).value
)

lazy val scalatestLibs = Seq(
  // @formatter:off
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  // @formatter:on
)

lazy val scalikejdbcLibs = Seq(
  // @formatter:off
  "org.scalikejdbc" %% "scalikejdbc"          % "2.3.+",
  "mysql"           %  "mysql-connector-java" % "5.1.38",
  "ch.qos.logback"  %  "logback-classic"      % "1.1.+"
  // @formatter:on
)

lazy val scalazCoreLib = "org.scalaz" %% "scalaz-core" % "7.2.7"
lazy val argonautLib = "io.argonaut" %% "argonaut" % "6.2"




//--------------------------------//
//       Project Settings         //
//--------------------------------//

lazy val common = project.
  settings(baseSettings).
  settings(
    name := "common",
    libraryDependencies ++= scalikejdbcLibs ++ scalatestLibs
  )

lazy val changeloggermanager = project.
  enablePlugins(JavaAppPackaging).
  dependsOn(common % "test->test;compile->compile").
  settings(baseSettings).settings(
  name := "changeloggermanager",
  resolvers += "bmjames Bintray Repo" at "https://dl.bintray.com/bmjames/maven",
  libraryDependencies ++= scalikejdbcLibs ++ scalatestLibs ++ Seq(
    scalazCoreLib,
    "net.bmjames" %% "scala-optparse-applicative" % "0.7"
  )
)

lazy val queuefeeder = project.
  dependsOn(common % "test->test", changeloggermanager % "test->compile").
  settings(baseSettings).
  settings(
    name := "queuefeeder",
    libraryDependencies ++= scalikejdbcLibs ++ scalatestLibs ++ Seq(
      "com.rabbitmq" % "amqp-client" % "4.1.0"
    )
  )

lazy val changelogtojson = project.
  dependsOn(common % "test->test;compile->compile").
  settings(baseSettings).
  settings(
    name := "dsl",
    libraryDependencies ++= scalikejdbcLibs ++ scalatestLibs ++ Seq(scalazCoreLib, argonautLib)
  )

lazy val main = project.
  dependsOn(common % "test->test;compile->compile").
  dependsOn(changeloggermanager).
  dependsOn(changelogtojson).
  settings(baseSettings).
  settings(
    name := "adenosyn",
    libraryDependencies ++= scalikejdbcLibs ++ scalatestLibs ++ Seq(
      scalazCoreLib,
      argonautLib,
      "com.sksamuel.elastic4s" %% "elastic4s-core" % "2.4.0"
    )
  )
