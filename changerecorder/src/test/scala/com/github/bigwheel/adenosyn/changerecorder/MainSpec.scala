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

  "with no options, exit status is not 0" in {
    val args = Array.empty[String]
    outputToDevNull {
      intercept[ExitException] { Main.main(args) }.status shouldNot be(0)
    }
  }

  {
    val arg = "--help"
    s"with '$arg', exit status is not 0" in {
      outputToDevNull {
        intercept[ExitException] { Main.main(arg.split(" ")) }.status should be(0)
      }
    }
  }

  {
    val arg = "validate"
    s"with '$arg', exit status is not 0" in {
      outputToDevNull {
        intercept[ExitException] { Main.main(arg.split(" ")) }.status shouldNot be(0)
      }
    }
  }

  {
    val arg = "setup"
    s"with '$arg', exit status is not 0" in {
      outputToDevNull {
        intercept[ExitException] { Main.main(arg.split(" ")) }.status shouldNot be(0)
      }
    }
  }

  {
    val arg = "teardown"
    s"with '$arg', exit status is not 0" in {
      outputToDevNull {
        intercept[ExitException] { Main.main(arg.split(" ")) }.status shouldNot be(0)
      }
    }
  }

  {
    val arg = s"setup ${sqlutil.url()} not_existing_db1 not_existing_db1 $userName $password"
    s"with '$arg', exit status is not 0" in {
      outputToDevNull {
        try {
          Main.main(arg.split(" "))
        } catch {
          case e: ExitException => e.status shouldNot be(0)
          case _: Throwable =>
        }
      }
    }
  }

  {
    val arg = s"setup ${sqlutil.url()} $observeeDbName $recordDbName $userName $password"
    s"with '$arg', exit status is 0" in {
      outputToDevNull {
        try {
          withTableUserAndDatabases { Main.main(arg.split(" ")) }
        } catch {
          case e: ExitException => e.status should be(0)
          case _: Throwable => fail()
        }
      }
    }
  }

}
