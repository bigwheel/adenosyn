package com.github.bigwheel.adenosyn.changeloggermanager

import com.github.bigwheel.adenosyn.DatabaseSpecHelper
import com.github.bigwheel.adenosyn.sqlutil
import java.io.FileOutputStream
import java.io.PrintStream
import org.scalatest.FreeSpec
import org.scalatest.Matchers

class MainSpec extends FreeSpec with Matchers with ExitStatusSpecHelper with DatabaseSpecHelper {

  def outputToLogFile[T](thunk: =>T): T = {
    val out = new PrintStream(new FileOutputStream("logs/test_out.log", true))
    val err = new PrintStream(new FileOutputStream("logs/test_err.log", true))
    try {
      Console.withOut(out) { Console.withErr(err) { thunk } }
    } finally {
      out.close
      err.close
    }
  }

  def exceptionOrExit1[T](thunk: =>T): Unit = try {
    thunk
  } catch {
    case e: ExitException => e.status shouldNot be(0)
    case _: Throwable =>
  }

  def noExceptionOrExit0[T](thunk: =>T): Unit = try {
    thunk
  } catch {
    case e: ExitException => e.status should be(0)
    case e: Throwable => e.printStackTrace(); fail(e)
  }

  "with no options, exit status is not 0" in {
    outputToLogFile {
      exceptionOrExit1 { Main.main(Array.empty[String]) }
    }
  }

  private[this] sealed case class TestCase(arg: String, isSuccess: Boolean,
    wrapper: (=> Unit) => Unit = arg => arg)

  private[this] val testCases = Seq(
    TestCase("--help", true),
    TestCase("validate", false),
    TestCase("setup", false),
    TestCase("teardown", false),
    TestCase(s"setup ${sqlutil.url()} unkown_db1 unkown_db2 $userName $password", false),
    TestCase(s"setup ${sqlutil.url()} $observeeDbName $changeLogDbName $userName $password", true,
      arg => withTableUserAndDatabases { arg }),
    TestCase(s"setup ${sqlutil.url()} $observeeDbName $changeLogDbName $userName $password -d", true,
      arg => withTableUserAndDatabases { arg }),
    TestCase(s"teardown ${sqlutil.url()} unkown_db1 unkown_db2 $userName $password", false),
    TestCase(s"teardown ${sqlutil.url()} $observeeDbName $changeLogDbName $userName $password", true,
      arg => withTableUserAndDatabases { arg }),
    TestCase(s"teardown ${sqlutil.url()} $observeeDbName $changeLogDbName $userName $password -d",
      true, arg => withTableUserAndDatabases { arg }),
    TestCase(s"validate ${sqlutil.url()} $observeeDbName $changeLogDbName $userName $password", true)
  )
  for (tc <- testCases)
    s"with '${tc.arg}', ${if (tc.isSuccess) "no " else "" }error happens" in {
      outputToLogFile {
        val checker = if (tc.isSuccess)
          noExceptionOrExit0 _
        else
          exceptionOrExit1 _

        checker { tc.wrapper { Main.main(tc.arg.split(" ")) } }
      }
    }

}
