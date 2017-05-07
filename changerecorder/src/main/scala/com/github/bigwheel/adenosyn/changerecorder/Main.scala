package com.github.bigwheel.adenosyn.changerecorder

import net.bmjames.opts._
import scalaz.syntax.applicativePlus._

object Main {

  sealed trait Mode
  val dbOptionParser: ((String, Int, String, String, Boolean) => Mode) => Parser[Mode] = ^^^^(
    strArgument(metavar("HOST")),
    intArgument(metavar("PORT")),
    strArgument(metavar("USERNAME")),
    strArgument(metavar("PASSWORD")),
    switch(long("dry-run"), help("only show sql queries"))
  ) _
  final case class Setup(host: String, port: Int, username: String, password: String,
    dryRun: Boolean) extends Mode
  val setup: Parser[Mode] = dbOptionParser(Setup)
  final case class Teardown(host: String, port: Int, username: String, password: String,
    dryRun: Boolean) extends Mode
  val teardown: Parser[Mode] = dbOptionParser(Teardown)
  final case class Validate(host: String, port: Int, username: String, password: String,
    dryRun: Boolean) extends Mode
  val validate: Parser[Mode] = dbOptionParser(Validate)

  val parser: Parser[Mode] = subparser(
    command("setup", info(setup, progDesc("create triggers and tables to record row changes"))),
    command("teardown", info(teardown, progDesc("remove triggers to record row changes"))),
    command("validate", info(validate, progDesc("validate trigger/table definitions")))
  )

  def main(args: Array[String]) {
    val opts = info(parser <*> helper,
      header("changerecorder - which records row changes in another table"))
    execParser(args, "changerecorder.jar", opts) match {
      case Validate(host, port, username, password, dryRun) =>
        println("not implemented yet")
      case Setup(host, port, username, password, dryRun) =>
        println("setup")
      case Teardown(host, port, username, password, dryRun) =>
        println("teardown")
    }
    sys.exit(0)
  }

}