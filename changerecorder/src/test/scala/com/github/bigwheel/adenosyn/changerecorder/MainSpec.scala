package com.github.bigwheel.adenosyn.changerecorder

import com.github.bigwheel.adenosyn.sqlutil
import java.io.File
import java.io.PrintStream
import org.scalatest.FreeSpec
import org.scalatest.Matchers

class MainSpec extends FreeSpec with Matchers with ExitStatusSpecHelper with DatabaseSpecHelper {

  def outputToDevNull[T](thunk: =>T): T = {
    val devNull1 = new PrintStream(new File("/dev/null"))
    val devNull2 = new PrintStream(new File("/dev/null"))
    try {
      Console.withOut(devNull1) { Console.withErr(devNull2) { thunk } }
    } finally {
      devNull1.close
      devNull2.close
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
    case e: Throwable => System.err.println(e.toString); fail(e)
  }

  "with no options, exit status is not 0" in {
    outputToDevNull {
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
    TestCase(s"setup ${sqlutil.url()} $observeeDbName $recordDbName $userName $password", true,
      arg => withTableUserAndDatabases { arg }),
    TestCase(s"setup ${sqlutil.url()} $observeeDbName $recordDbName $userName $password -d", true,
      arg => withTableUserAndDatabases { arg }),
    TestCase(s"teardown ${sqlutil.url()} unkown_db1 unkown_db2 $userName $password", true),
    TestCase(s"teardown ${sqlutil.url()} $observeeDbName unknown_db2 $userName $password", true),
    TestCase(s"teardown ${sqlutil.url()} $observeeDbName $recordDbName $userName $password", true,
      arg => withTableUserAndDatabases { arg }),
    TestCase(s"teardown ${sqlutil.url()} $observeeDbName $recordDbName $userName $password -d",
      true, arg => withTableUserAndDatabases { arg }),
    TestCase(s"validate ${sqlutil.url()} $observeeDbName $recordDbName $userName $password", true)
  )
  for (tc <- testCases)
    s"with '${tc.arg}', ${if (tc.isSuccess) "no " else "" }error happens" in {
      outputToDevNull {
        val checker = if (tc.isSuccess)
          noExceptionOrExit0 _
        else
          exceptionOrExit1 _

        checker { tc.wrapper { Main.main(tc.arg.split(" ")) } }
      }
    }

}
