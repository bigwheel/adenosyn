package com.github.bigwheel.adenosyn.changerecorder

import org.scalatest.FreeSpec
import org.scalatest.Matchers

class MainSpec extends FreeSpec with Matchers with ExitStatusSpec {

  "with no options, exit status is not 0" in {
    val args = Array.empty[String]
    intercept[ExitException] { Main.main(args) }.status shouldNot be(0)
  }

  "with '--help', exit status is 0" in {
    val args = "--help".split(" ")
    intercept[ExitException] { Main.main(args) }.status should be(0)
  }

  "with 'validate', exit status is 0" in {
    val args = "validate".split(" ")
    intercept[ExitException] { Main.main(args) }.status should be(0)
  }

  {
    val arg = "setup"
    s"with '$arg', exit status is not 0" in {
      intercept[ExitException] { Main.main(arg.split(" ")) }.status shouldNot be(0)
    }
  }

  {
    val arg = "setup abc 10 def ghi"
    s"with '$arg', exit status is 0" in {
      intercept[ExitException] { Main.main(arg.split(" ")) }.status should be(0)
    }
  }

}
