package com.github.bigwheel.adenosyn.changeupdater

import com.github.bigwheel.adenosyn.changerecorder.ChangeRecorder
import com.github.bigwheel.adenosyn.sqlutil
import com.github.bigwheel.adenosyn.structure._
import com.sksamuel.elastic4s.ElasticClient
import com.sksamuel.elastic4s.ElasticsearchClientUri
import org.scalatest.FunSpec
import org.scalatest.Matchers
import scalikejdbc.ConnectionPool
import scalikejdbc.DB
import scalikejdbc.NamedDB
import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s.IndexAndType
import scalaz.Scalaz._

class MainSpec extends FunSpec with Matchers {

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
      new Subject(sqlutil.url("observee"), sqlutil.url("record"), "adenosyn", "yb",
        sqlutil.elasticsearchUrl, Seq((structure, IndexAndType("index1", "type1"))))

    }
  }

  class Subject(observeeDbUrl: String, recordDbUrl: String, user: String, password: String,
    elasticsearchClientUri: ElasticsearchClientUri, mappings: Seq[(JsValue, IndexAndType)]) {

    val cr = new ChangeRecorder(observeeDbUrl, recordDbUrl, user, password)
    val client = ElasticClient.transport(elasticsearchClientUri)

    client.close
  }

}
