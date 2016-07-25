name := "util"
version := "0.1.0"
scalaVersion := "2.11.8"
scalacOptions ++= Seq("-unchecked", "-deprecation")
// @formatter:off
libraryDependencies ++= Seq(
  "org.scalikejdbc"   %% "scalikejdbc"          % "2.3.+",
  "mysql"             %  "mysql-connector-java" % "5.1.38",
  "ch.qos.logback"    %  "logback-classic"      % "1.1.+"
)
// @formatter:on