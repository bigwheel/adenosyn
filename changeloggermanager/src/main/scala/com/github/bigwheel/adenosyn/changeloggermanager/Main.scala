package com.github.bigwheel.adenosyn.changeloggermanager

import net.bmjames.opts._
import scalaz.syntax.applicativePlus._

object Main {

  sealed trait Mode
  val dbOptionParser: ((String, String, String, String, String, Boolean) => Mode) => Parser[Mode] = ^^^^^( // unfortunately follow help texts for strArgument don't appear in any help text
    strArgument(metavar("URL"), help("JDBC URL without specifying schema")),
    strArgument(metavar("OBSERVEE"), help("observee database name")),
    strArgument(metavar("CHANGELOG"), help("changelog database name")),
    strArgument(metavar("USERNAME"), help("username to connect databases")),
    strArgument(metavar("PASSWORD"), help("password to connect databases")),
    switch(long("dry-run"), short('d'), help("only show sql queries"))
  ) _
  final case class Setup(url: String, observee: String, changeLog: String,
    username: String, password: String, dryRun: Boolean) extends Mode
  val setup: Parser[Mode] = dbOptionParser(Setup)
  final case class Teardown(url: String, observee: String, changeLog: String,
    username: String, password: String, dryRun: Boolean) extends Mode
  val teardown: Parser[Mode] = dbOptionParser(Teardown)
  final case class Validate(url: String, observee: String, changeLog: String,
    username: String, password: String, dryRun: Boolean) extends Mode
  val validate: Parser[Mode] = dbOptionParser(Validate)

  val parser: Parser[Mode] = subparser(
    command("setup", info(setup,
      progDesc("create triggers and tables to changelog row changes"))),
    command("teardown", info(teardown,
      progDesc("remove triggers to changelog row changes"))),
    command("validate", info(validate,
      progDesc("validate trigger/table definitions")))
  )

  def main(args: Array[String]) {
    val opts = info(parser <*> helper,
      header("changeloggermanager - which changelogs row changes in another table"))
    execParser(args, "changeloggermanager.jar", opts) match {
      case Validate(url, observee, changeLog, username, password, dryRun) =>
        println("not implemented yet")
      case Setup(url, observee, changeLog, username, password, dryRun) =>
        val cr = new ChangeLoggerManager(url, observee, changeLog, username,
          password)
        if (dryRun)
          println((cr.setUpQueries.forObservee ++ cr.setUpQueries.forChangeLog).mkString("\n"))
        else
          cr.setUp()
      case Teardown(url, observee, changeLog, username, password, dryRun) =>
        val cr = new ChangeLoggerManager(url, observee, changeLog, username, password)
        if (dryRun)
          println(cr.tearDownQueries.mkString("\n"))
        else
          cr.tearDown()
    }
    sys.exit(0)
  }

}
