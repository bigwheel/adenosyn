package com.github.bigwheel.adenosyn.queuefeeder

import com.github.bigwheel.adenosyn.DatabaseSpecHelper
import com.github.bigwheel.adenosyn.changerecorder.ChangeRecorder
import com.github.bigwheel.adenosyn.sqlutil
import com.rabbitmq.client.ConnectionFactory
import org.scalatest.FreeSpec
import org.scalatest.Matchers

class QueueFeederSpec extends FreeSpec with Matchers with DatabaseSpecHelper {

  "base test" in {
    withTableUserAndDatabases {
      new ChangeRecorder(sqlutil.url(), observeeDbName, recordDbName,
        userName, password).setUp

      val cf = new ConnectionFactory
      val connection = cf.newConnection()
      val channel = connection.createChannel()
      val queueName = "q"

      channel.queueDeclare(queueName, false, false, false, null)
      val messageBodyBytes = "Hello, world!".getBytes("UTF-8")
      channel.basicPublish("", queueName, null, messageBodyBytes)

      println(channel.messageCount(queueName))

      channel.close()
      connection.close()

      1 should be(1)
    }
  }

}
