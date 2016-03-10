name := "youseibox"
version := "0.1.0"
scalaVersion := "2.11.7"
scalacOptions ++= Seq("-unchecked", "-deprecation")
libraryDependencies ++= Seq(
  "org.scalactic"   %% "scalactic"            % "2.2.6",
  "org.scalatest"   %% "scalatest"            % "2.2.6" % "test",
  "org.scalikejdbc" %% "scalikejdbc"          % "2.3.+",
  "mysql"           %  "mysql-connector-java" % "5.1.38",
  "ch.qos.logback"  %  "logback-classic"      % "1.1.+",
  "org.scalaz"      %% "scalaz-core"          % "7.1.1",
  "io.argonaut"     %% "argonaut"             % "6.1"
)
