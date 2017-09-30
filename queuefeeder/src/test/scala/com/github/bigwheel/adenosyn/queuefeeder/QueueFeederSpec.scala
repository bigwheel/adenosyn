package com.github.bigwheel.adenosyn.queuefeeder

import com.github.bigwheel.adenosyn.DatabaseSpecHelper
import com.github.bigwheel.adenosyn.Using.using
import com.rabbitmq.client.ConnectionFactory
import dispatch.Defaults._
import dispatch._
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import scala.concurrent.Await
import scala.concurrent.duration._
import argonaut._
import Argonaut._
import com.rabbitmq.client.Channel
import org.scalatest.BeforeAndAfter

class QueueFeederSpec extends FreeSpec with Matchers with BeforeAndAfter
  with DatabaseSpecHelper {

  before {
    withChannel(deleteAllQueues)
  }

  private[this] def fetchAllQueueNames = {
    val query = url("http://localhost:15672/api/queues").
      as_!("guest", "guest")
    val request = Http.default(query OK as.String)
    val responseBody = Await.result(request, 10.seconds)
    val json = responseBody.parseOption.get
    JsonPath.root.each.name.string.getAll(json)
  }

  private[this] def deleteAllQueues(channel: Channel) =
    fetchAllQueueNames.foreach(channel.queueDelete)

  private[this] def withChannel[T](f: Channel => T): T =
    using(new ConnectionFactory().newConnection) { connection =>
      using(connection.createChannel)(f)
    }

  "base test" in {
    withTableUserAndDatabases {
      withChannel { channel =>
        val queueName = "q"

        channel.queueDeclare(queueName, false, false, false, null)
        val messageBodyBytes = "Hello, world!".getBytes("UTF-8")
        channel.basicPublish("", queueName, null, messageBodyBytes)

        channel.messageCount(queueName) should be(1)
      }
    }
  }

}
