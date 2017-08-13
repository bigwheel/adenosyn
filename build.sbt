//--------------------------------//
//       Shared Settings          //
//--------------------------------//

lazy val baseSettings = Seq(
  organization := "com.github.bigwheel",
  version := "1.0.0",
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
  test in assembly := {}
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

lazy val changerecorder = project.
  enablePlugins(JavaAppPackaging).
  dependsOn(common % "test->test;compile->compile").
  settings(baseSettings).settings(
  name := "changerecorder",
  resolvers += "bmjames Bintray Repo" at "https://dl.bintray.com/bmjames/maven",
  libraryDependencies ++= scalikejdbcLibs ++ scalatestLibs ++ Seq(
    scalazCoreLib,
    "net.bmjames" %% "scala-optparse-applicative" % "0.7"
  )
)

lazy val queuefeeder = project.
  dependsOn(common % "test->test", changerecorder % "test->compile").
  settings(baseSettings).
  settings(
    name := "queuefeeder",
    libraryDependencies ++= scalikejdbcLibs ++ scalatestLibs
  )

lazy val recordstojson = project.
  dependsOn(common % "test->test;compile->compile").
  settings(baseSettings).
  settings(
    name := "dsl",
    libraryDependencies ++= scalikejdbcLibs ++ scalatestLibs ++ Seq(scalazCoreLib, argonautLib)
  )

lazy val main = project.
  dependsOn(common % "test->test;compile->compile").
  dependsOn(changerecorder).
  dependsOn(recordstojson).
  settings(baseSettings).
  settings(
    name := "adenosyn",
    libraryDependencies ++= scalikejdbcLibs ++ scalatestLibs ++ Seq(
      scalazCoreLib,
      argonautLib,
      "com.sksamuel.elastic4s" %% "elastic4s-core" % "2.4.0"
    )
  )
