package com.github.bigwheel.adenosyn.changeupdater

import com.github.bigwheel.adenosyn.DatabaseSpecHelper
import com.github.bigwheel.adenosyn.changeloggermanager.ChangeLoggerManager
import com.github.bigwheel.adenosyn.changelogtojson.Assembler
import com.github.bigwheel.adenosyn.changelogtojson.dsl._
import com.github.bigwheel.adenosyn.sqlutil
import com.sksamuel.elastic4s.ElasticClient
import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s.ElasticsearchClientUri
import com.sksamuel.elastic4s.IndexAndType
import com.sksamuel.elastic4s.source.JsonDocumentSource
import org.elasticsearch.common.settings.Settings
import org.scalatest.BeforeAndAfter
import org.scalatest.BeforeAndAfterAll
import org.scalatest.FunSpec
import org.scalatest.Matchers
import scalaz.Scalaz._

class MainSpec extends FunSpec with Matchers with BeforeAndAfter with BeforeAndAfterAll with DatabaseSpecHelper {

  private[this] val elasticsearchUrl = ElasticsearchClientUri("127.0.0.1", 9300)
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

  it("a") {
    withUserAndDatabases {
      autoCommit(observeeDbConnectionPool) { session =>
        sqlutil.executeScript("/fixture.sql")(session)
      }
      val structure = JsObject(
        RootJoinDefinition(
          Table(
            "artist",
            JoinDefinition("id" -> "Int", false, "artist_id" -> "Int", Table("artist_kana"))
          )
        ).some,
        Map[String, JsValue](
          "_id" -> JsString("artist", "id"),
          "name" -> JsString("artist", "name"),
          "kana" -> JsString("artist_kana", "kana")
        )
      )
      val subject = new Subject(elasticsearchUrl, Seq((structure, IndexAndType("index1", "type1"))))
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

  class Subject(elasticsearchClientUri: ElasticsearchClientUri,
    mappings: Seq[(JsValue, IndexAndType)]) {
    private[this] val url = sqlutil.url()

    val cr = new ChangeLoggerManager(url, observeeDbName, changeLogDbName,
      userName, password)

    def buildAll() = {
      val client = ElasticClient.transport(
        Settings.settingsBuilder.put("cluster_name", "elasticsearch").build(),
        elasticsearchUrl
      )

      usingDb() { db =>
        db.conn.setCatalog(observeeDbName)
        db.autoCommit { implicit session =>
          val a = new Assembler
          val json = a.AssembleAll(mappings.head._1).head
          val noIdJson = json.hcursor.downField("_id").delete.undo.get
          val idString = json.field("_id").get.string.get
          client.execute {
            index into mappings.head._2 doc JsonDocumentSource(noIdJson.nospaces) id idString
          }.await
        }
      }


      client.close
    }
  }

}
