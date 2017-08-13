package com.github.bigwheel.adenosyn.queuefeeder

import com.github.bigwheel.adenosyn.DatabaseSpecHelper
import com.github.bigwheel.adenosyn.changerecorder.ChangeRecorder
import com.github.bigwheel.adenosyn.sqlutil
import org.scalatest.FreeSpec
import org.scalatest.Matchers

class QueueFeederSpec extends FreeSpec with Matchers with DatabaseSpecHelper {

  "base test" in {
    withTableUserAndDatabases {
      new ChangeRecorder(sqlutil.url(), observeeDbName, recordDbName,
        userName, password).setUp
      1 should be(1)
    }
  }

}
