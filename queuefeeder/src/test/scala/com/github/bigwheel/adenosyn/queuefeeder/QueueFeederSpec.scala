package com.github.bigwheel.adenosyn.queuefeeder

import com.github.bigwheel.adenosyn.DatabaseSpecHelper
import org.scalatest.FreeSpec
import org.scalatest.Matchers

class QueueFeederSpec extends FreeSpec with Matchers with DatabaseSpecHelper {

  "base test" in {
    withTableUserAndDatabases { 1 should be(1) }
  }

}
