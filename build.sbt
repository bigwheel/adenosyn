name := "adenosyn"
version := "1.0.0"
scalaVersion := "2.11.8"
scalacOptions ++= Seq("-unchecked", "-deprecation")
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
// @formatter:off
libraryDependencies ++= Seq(
  "org.scalactic"          %% "scalactic"            % "3.0.1",
  "org.scalatest"          %% "scalatest"            % "3.0.1" % "test",
  "org.scalikejdbc"        %% "scalikejdbc"          % "2.3.+",
  "mysql"                  %  "mysql-connector-java" % "5.1.38",
  "ch.qos.logback"         %  "logback-classic"      % "1.1.+",
  "org.scalaz"             %% "scalaz-core"          % "7.1.1",
  "io.argonaut"            %% "argonaut"             % "6.1",
  "com.sksamuel.elastic4s" %% "elastic4s-core"       % "2.4.0"
)
// @formatter:on
lazy val changerecorder = project
lazy val root = (project in file("./")).dependsOn(changerecorder)
