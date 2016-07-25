name := "youseibox"
version := "0.1.0"
scalaVersion := "2.11.8"
scalacOptions ++= Seq("-unchecked", "-deprecation")
// @formatter:off
lazy val changeRecorder = project.in(file("change-recorder"))
lazy val changeUpdater  = project.in(file("change-updater"))
lazy val jsonAssembler  = project.in(file("json-assembler"))
lazy val util           = project.in(file("util"))
// @formatter:on
