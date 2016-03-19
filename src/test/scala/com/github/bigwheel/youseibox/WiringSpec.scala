package com.github.bigwheel.youseibox

import argonaut.Argonaut._
import argonaut._
import com.github.bigwheel.youseibox.json._
import com.github.bigwheel.youseibox.table._
import org.scalatest.FunSpec
import org.scalatest.Matchers
import scala.sys.process.Process
import scalaz.Scalaz._
import scalikejdbc._

class WiringSpec extends FunSpec with Matchers {
  val ipAddress = Process("otto dev address").!!.stripLineEnd

  Class.forName("com.mysql.jdbc.Driver")
  ConnectionPool.singleton(
    s"jdbc:mysql://$ipAddress/youseibox_test?characterEncoding=UTF-8", "youseibox", "youseibox")
  implicit val session = AutoSession

  def createFixtures() = {
    DB autoCommit { implicit session =>
      Seq(
        "drop table if exists artist",
        "drop table if exists artist_kana",
        "drop table if exists music",
        "drop table if exists content"
      ).foreach(SQL(_).execute.apply())

      Seq(
        """
        create table artist (
          id   INT  NOT NULL PRIMARY KEY AUTO_INCREMENT,
          name TEXT NOT NULL
        )
        """,
        """
        create table artist_kana (
          artist_id INT  NOT NULL PRIMARY KEY,
          kana      TEXT NOT NULL
        )
        """,
        """
        create table music (
          id        INT  NOT NULL PRIMARY KEY AUTO_INCREMENT,
          artist_id INT  NOT NULL,
          name      TEXT NOT NULL,
          INDEX index_artist_id(artist_id)
        )
        """,
        """
        create table content (
          id       INT  NOT NULL PRIMARY KEY AUTO_INCREMENT,
          music_id INT  NOT NULL,
          name     TEXT NOT NULL,
          INDEX index_music_id(music_id)
        )
        """
      ).
        foreach(SQL(_).execute.apply())

      Seq(
        """INSERT INTO artist(id, name) VALUES (1, "水樹奈々")""",
        """INSERT INTO artist_kana(artist_id, kana) VALUES (1, "みずきなな")""",
        """INSERT INTO music(id, artist_id, name) VALUES (11, 1, "深愛")""",
        """INSERT INTO music(id, artist_id, name) VALUES (12, 1, "innocent starter")""",
        """INSERT INTO content(id, music_id, name) VALUES (111, 11, "深愛 - ショートVer.")""",
        """INSERT INTO content(id, music_id, name) VALUES (112, 11, "深愛 - ロングVer.")""",
        """INSERT INTO content(id, music_id, name) VALUES (121, 12, "innocent starter(inst)")"""
      ).foreach(SQL(_).execute.apply())
    }
  }

  createFixtures

  it("開発環境VMが動いている") {
    val result = Process("otto dev vagrant status").!!
    result.split("\n")(3) should
      equal("default                   running (virtualbox)")
  }

  it("開発環境のIPアドレスが正しく取得できる") {
    ipAddress should fullyMatch regex """\A\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}\Z"""
  }

  def asJsonObj(obj: List[Map[String, Any]]): List[Map[String, Any]] = {
    obj.map { m =>
      println(m("json").asInstanceOf[String])
      m.updated("json", m("json").asInstanceOf[String].parseOption.get)
    }
  }

  val tests = Seq(
    (
      "最も単純なjsonオブジェクトを組み立てられる",
      JsObject(
        RootTable(
          "artist"
        ).some,
        Map[String, JsValue](
          "name" -> JsString("artist.name")
        )
      ),
      List(Map("json" -> Json("name" := "水樹奈々")))
    ),
    (
      "複数プロパティのjsonオブジェクトを組み立てられる",
      JsObject(
        RootTable(
          "artist"
        ).some,
        Map[String, JsValue](
          "id" -> JsInt("artist.id"),
          "name" -> JsString("artist.name")
        )
      ),
      List(Map("json" -> Json("id" := 1, "name" := "水樹奈々")))
    ),
    (
      "単純チェインのテーブルJOINでjsonオブジェクトを組み立てられる",
      JsObject(
        RootTable(
          "artist",
          _1to1Table(
            "artist_kana",
            "artist.id = artist_kana.artist_id"
          ).some
        ).some,
        Map[String, JsValue](
          "name" -> JsString("artist.name"),
          "kana" -> JsString("artist_kana.kana")
        )
      ),
      List(Map("json" -> Json("name" := "水樹奈々", "kana" := "みずきなな")))
    ),
    (
      "jsonオブジェクトがネストしていても組み立てられる",
      JsObject(
        RootTable(
          "artist"
        ).some,
        Map[String, JsValue](
          "name" -> JsString("artist.name"),
          "musics" -> JsArray(
            _1toNTable(
              "music",
              "artist_id",
              "artist.id"
            ).some,
            JsString("music.name")
          )
        )
      ),
      List(Map(
        "json" -> Json(
          "name" := "水樹奈々",
          "musics" := Json.array(jString("深愛"), jString("innocent starter"))
        )
      ))
    ),
    (
      "ネストと直列JOINが両方あっても組み立てられる",
      JsObject(
        RootTable(
          "artist",
          _1to1Table(
            "artist_kana",
            "artist.id = artist_kana.artist_id"
          ).some
        ).some,
        Map[String, JsValue](
          "name" -> JsString("artist.name"),
          "kana" -> JsString("artist_kana.kana"),
          "musics" -> JsArray(
            _1toNTable(
              "music",
              "artist_id",
              "artist.id"
            ).some,
            JsString("music.name")
          )
        )
      ),
      List(Map(
        "json" -> Json(
          "name" := "水樹奈々",
          "kana" := "みずきなな",
          "musics" := Json.array(jString("深愛"), jString("innocent starter"))
        )
      ))
    ),
    (
      "ネスト内からネスト外のカラムを参照できる",
      JsObject(
        RootTable(
          "artist"
        ).some,
        Map[String, JsValue](
          "name" -> JsString("artist.name"),
          "musics" -> JsArray(
            _1toNTable(
              "music",
              "artist_id",
              "artist.id"
            ).some,
            JsInt("artist.id")
          )
        )
      ),
      List(Map(
        "json" -> Json(
          "name" := "水樹奈々",
          "musics" := Json.array(jNumber(1), jNumber(1))
        )
      ))
    ),
    (
      "jsonオブジェクトが配列の中だとしても組み立てられる",
      JsObject(
        RootTable(
          "artist"
        ).some,
        Map[String, JsValue](
          "name" -> JsString("artist.name"),
          "musics" -> JsArray(
            _1toNTable(
              "music",
              "artist_id",
              "artist.id"
            ).some,
            JsObject(None, Map[String, JsValue]("name" -> JsString("music.name")))
          )
        )
      ),
      List(Map(
        "json" -> Json(
          "name" := "水樹奈々",
          "musics" := Json.array(Json("name" := "深愛"), Json("name" := "innocent starter"))
        )
      ))
    ),
    (
      "ネストが二重でも組み立てられる",
      JsObject(
        RootTable(
          "artist"
        ).some,
        Map[String, JsValue](
          "name" -> JsString("artist.name"),
          "musics" -> JsArray(
            _1toNTable(
              "music",
              "artist_id",
              "artist.id"
            ).some,
            JsObject(
              None,
              Map[String, JsValue](
                "name" -> JsString("music.name"),
                "contents" -> JsArray(
                  _1toNTable(
                    "content",
                    "music_id",
                    "music.id"
                  ).some,
                  JsObject(
                    None,
                    Map[String, JsValue](
                      "name" -> JsString("content.name")
                    )
                  )
                )
              )
            )
          )
        )
      ),
      List(Map(
        "json" -> Json(
          "name" := "水樹奈々",
          "musics" := Json.array(
            Json(
              "name" := "深愛",
              "contents" := Json.array(
                Json("name" := "深愛 - ショートVer."),
                Json("name" := "深愛 - ロングVer.")
              )
            ),
            Json(
              "name" := "innocent starter",
              "contents" := Json.array(
                Json("name" := "innocent starter(inst)")
              )
            )
          )
        )
      ))
    )
  )

  for (test <- tests) {
    it(test._1) {
      val sqls = test._2.toSql
      println(sqls)
      sqls.preProcess.foreach(SQL(_).execute.apply())
      try {
        SQL(sqls.selectMain).map(_.toMap).list.apply() |> asJsonObj should equal(test._3)
      } finally {
        sqls.postProcess.foreach(SQL(_).execute.apply())
      }
    }
  }

  it ("Optionで型を指定しているようなデータ構造はきっちりNoneを渡した時のテストも書いておくこと") {
    pending
  }
}
