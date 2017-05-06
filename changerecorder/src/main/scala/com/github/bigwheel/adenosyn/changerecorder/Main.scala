package com.github.bigwheel.adenosyn.changerecorder

import net.bmjames.opts._
import scalaz.syntax.applicativePlus._

object Main {

  sealed trait Mode
  final case class Setup(host: String, port: Int, username: String, password: String,
    dryRun: Boolean) extends Mode
  val setup: Parser[Mode] =
    ^^^^(
      strArgument(metavar("HOST")),
      intArgument(metavar("PORT")),
      strArgument(metavar("USERNAME")),
      strArgument(metavar("PASSWORD")),
      switch(long("dry-run"), help("only show sql queries"))
    )(Setup)
  final case class Teardown(host: String, port: Int, username: String, password: String,
    dryRun: Boolean) extends Mode
  val teardown: Parser[Mode] =
    ^^^^(
      strArgument(metavar("HOST")),
      intArgument(metavar("PORT")),
      strArgument(metavar("USERNAME")),
      strArgument(metavar("PASSWORD")),
      switch(long("dry-run"), help("only show sql queries"))
    )(Teardown)
  final case class Validate(host: String, port: Int, username: String, password: String,
    dryRun: Boolean) extends Mode
  val validate: Parser[Mode] =
    ^^^^(
      strArgument(metavar("HOST")),
      intArgument(metavar("PORT")),
      strArgument(metavar("USERNAME")),
      strArgument(metavar("PASSWORD")),
      switch(long("dry-run"), help("only show sql queries"))
    )(Validate)

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
        sys.exit(0)
      case Setup(host, port, username, password, dryRun) =>
        println("setup")
        sys.exit(0)
      case Teardown(host, port, username, password, dryRun) =>
        println("teardown")
        sys.exit(0)
    }
  }

}
