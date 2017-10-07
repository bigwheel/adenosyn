package com.github.bigwheel.adenosyn.queuefeeder

import argonaut.Argonaut._
import argonaut._
import com.github.bigwheel.adenosyn.DatabaseSpecHelper
import com.github.bigwheel.adenosyn.Using.using
import com.github.bigwheel.adenosyn.sqlutil.RichString
import com.rabbitmq.client.Channel
import com.rabbitmq.client.ConnectionFactory
import com.rabbitmq.client.DefaultConsumer
import com.rabbitmq.client.Envelope
import dispatch.Defaults._
import dispatch._
import org.scalatest.BeforeAndAfter
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import scala.concurrent.Await
import scala.concurrent.duration._
import scalikejdbc._
import com.rabbitmq.client.AMQP
import org.mockito.Mockito._
import org.mockito.ArgumentMatchers._

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
      autoCommit(defaultDbConnectionPool) { implicit session =>
        (s"CREATE TABLE $changeLogDbName.table1(" +
          "pr1 INTEGER not null, " +
          "pr2 VARCHAR(30) not null, " +
          "updated_at TIMESTAMP not null, " +
          "PRIMARY KEY(pr1, pr2))").query
      }

      using(DB(defaultDbConnectionPool.borrow)) { db =>
        db.autoClose(false)
        // http://stackoverflow.com/a/13433382/4006322
        db.conn.setCatalog(changeLogDbName)

        db.getTableNames()
      }.map { tableName =>
        sql"SELECT * FROM $tableName"
      }

      withChannel { channel =>
        val queueName = "q"

        channel.queueDeclare(queueName, false, false, false, null)

        val messageBodyBytes = "Hello, world!".getBytes("UTF-8")
        channel.basicPublish("", queueName, null, messageBodyBytes)

        /*channel.basicConsume(queueName, true, new DefaultConsumer(channel){
          override def handleDelivery(consumerTag: JsonField,
            envelope: Envelope,
            properties: AMQP.BasicProperties,
            body: Array[Byte]): Unit = {
            val message = new String(body, "UTF-8")
            System.out.println(" [x] Received '" + message + "'")
          }
        })*/
        val m = mock(classOf[DefaultConsumer])
        channel.basicConsume(queueName, true, m)


        Thread.sleep(10 * 1000L)

        verify(m).handleDelivery(anyString(), any[Envelope], any[AMQP
        .BasicProperties], any[Array[Byte]])
        //channel.messageCount(queueName) should be(1)
      }
    }
  }

}
