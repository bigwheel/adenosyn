package com.github.bigwheel.adenosyn.changeupdater

import com.github.bigwheel.adenosyn.changerecorder.ChangeRecorder
import com.github.bigwheel.adenosyn.changerecorder.JdbcUrl
import com.github.bigwheel.adenosyn.dsl._
import com.github.bigwheel.adenosyn.sqlutil
import com.sksamuel.elastic4s.ElasticClient
import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s.ElasticsearchClientUri
import com.sksamuel.elastic4s.IndexAndType
import com.sksamuel.elastic4s.source.JsonDocumentSource
import org.elasticsearch.common.settings.Settings
import org.scalatest.BeforeAndAfter
import org.scalatest.FunSpec
import org.scalatest.Matchers
import scalaz.Scalaz._
import scalikejdbc.Commons2ConnectionPoolFactory
import scalikejdbc.ConnectionPool
import scalikejdbc.DB
import scalikejdbc.NamedDB

class MainSpec extends FunSpec with Matchers with BeforeAndAfter {

  val elasticsearchUrl = ElasticsearchClientUri("127.0.0.1", 9300)
  private[this] val client = ElasticClient.transport(
    Settings.settingsBuilder.put("cluster_name", "elasticsearch").build(), elasticsearchUrl
  )

  before {
    client.execute {
      deleteIndex("_all")
    }.await
  }

  after {
    client.close
  }

  sqlutil.suppressLog()

  Class.forName("com.mysql.jdbc.Driver")
  ConnectionPool.singleton(sqlutil.url(), "root", "root")
  ConnectionPool.add('observee, sqlutil.url("observee"), "root", "root")
  ConnectionPool.add('record, sqlutil.url("record"), "root", "root")

  private[this] def withDatabases(test: => Any) {
    DB.autoCommit { implicit session =>
      sqlutil.executeStatements(
        """DROP DATABASE IF EXISTS observee;
          |CREATE DATABASE         observee;
          |DROP DATABASE IF EXISTS record;
          |CREATE DATABASE         record;
          |DROP USER IF EXISTS 'adenosyn'@'%';
          |CREATE USER         'adenosyn'@'%' IDENTIFIED BY 'yb';
          |GRANT ALL ON observee.* TO 'adenosyn'@'%';
          |GRANT ALL ON record.*   TO 'adenosyn'@'%';""".stripMargin
      )
    }
    NamedDB('observee).autoCommit { implicit session =>
      sqlutil.executeScript("/fixture.sql")
    }

    test
  }

  it("a") {
    withDatabases {
      val structure = JsObject(
        RootJoinDefinition(
          Table(
            "artist",
            JoinDefinition("id" -> "Int", false, "artist_id" -> "Int", Table("artist_kana"))
          )
        ).some,
        Map[String, JsValue](
          "name" -> JsString("artist", "name"),
          "kana" -> JsString("artist_kana", "kana")
        )
      )
      val subject = new Subject(sqlutil.jdbcUrl("observee"), sqlutil.jdbcUrl("record"), "adenosyn", "yb",
        elasticsearchUrl, Seq((structure, IndexAndType("index1", "type1"))))
      subject.buildAll
      client.execute {
        flush index "index1"
      }.await
      val response = client.execute {
        search in "index1" -> "type1"
      }.await
      response.hits.map(_.id) should be(Array("1"))
    }
  }

  class Subject(observeeDbUrl: JdbcUrl, recordDbUrl: JdbcUrl, user: String, password: String,
    elasticsearchClientUri: ElasticsearchClientUri, mappings: Seq[(JsValue, IndexAndType)]) {

    val cr = new ChangeRecorder(observeeDbUrl, recordDbUrl, user, password)

    def buildAll() = {
      val client = ElasticClient.transport(Settings.settingsBuilder.put("cluster_name",
        "elasticsearch").build(), elasticsearchUrl)

      val pool = Commons2ConnectionPoolFactory(observeeDbUrl.plainUrl, user, password)
      scalikejdbc.using(DB(pool.borrow)) { observeeDb =>
        observeeDb.autoCommit { implicit session =>
          val jsons = fetchJsonResult(mappings.head._1)(session)
          client.execute {
            index into mappings.head._2 doc JsonDocumentSource(jsons.head.nospaces)
          }.await
        }
      }


      client.close
    }
  }

}
