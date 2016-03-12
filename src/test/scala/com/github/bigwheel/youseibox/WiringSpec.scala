package com.github.bigwheel.youseibox

import com.github.bigwheel.youseibox
import org.scalatest.FunSpec
import org.scalatest.Matchers
import scala.sys.process.Process
import scalaz._, Scalaz._
import scalikejdbc._
import argonaut._, Argonaut._

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
    obj.map { m => m.updated("json", m("json").asInstanceOf[String].parseOption.get) }
  }

  it("最も単純なjsonオブジェクトを組み立てられる") {
    val subject = youseibox.JsonObject(
      RootTableDefinition(
        "artist"
      ).some,
      Map[String, JsonValue](
        "name" -> JsonString("artist.name")
      )
    )
    SQL(subject.toSql.selectMain).map(_.toMap).list.apply() |> asJsonObj should equal(
      List(Map("id" -> 1, "name" -> "水樹奈々",
        "json" -> Json("name" := "水樹奈々")
      ))
    )
  }

  it("複数プロパティのjsonオブジェクトを組み立てられる") {
    val subject = youseibox.JsonObject(
      RootTableDefinition(
        "artist"
      ).some,
      Map[String, JsonValue](
        "id" -> JsonInt("artist.id"),
        "name" -> JsonString("artist.name")
      )
    )
    SQL(subject.toSql.selectMain).map(_.toMap).list.apply() |> asJsonObj should equal(
      List(Map("id" -> 1, "name" -> "水樹奈々",
        "json" -> Json("id" := 1, "name" := "水樹奈々")
      ))
    )
  }

  it("単純チェインのテーブルJOINでjsonオブジェクトを組み立てられる") {
    val subject = youseibox.JsonObject(
      RootTableDefinition(
        "artist",
        LeafOneToOneTableDefinition(
          "artist_kana",
          "artist.id = artist_kana.artist_id"
        ).some
      ).some,
      Map[String, JsonValue](
        "name" -> JsonString("artist.name"),
        "kana" -> JsonString("artist_kana.kana")
      )
    )
    SQL(subject.toSql.selectMain).map(_.toMap).list.apply() |> asJsonObj should equal(
      List(Map("id" -> 1, "name" -> "水樹奈々",
        "json" -> Json("name" := "水樹奈々", "kana" := "みずきなな")
      ))
    )
  }

  it("jsonオブジェクトがネストしていても組み立てられる") {
    val subject = youseibox.JsonObject(
      RootTableDefinition(
        "artist"
      ).some,
      Map[String, JsonValue](
        "name" -> JsonString("artist.name"),
        "musics" -> JsonArray(
          LeafOneToManyTableDefinition(
            "music",
            "artist.id = music.artist_id",
            "artist.id"
          ).some,
          JsonString("music.name")
        )
      )
    )
    SQL(subject.toSql.selectMain).map(_.toMap).list.apply() |> asJsonObj should equal(
      List(Map("id" -> 1, "name" -> "水樹奈々",
        "json" -> Json(
          "name" := "水樹奈々",
          "musics" := Json.array(jString("深愛"), jString("innocent starter"))
        )
      ))
    )
  }

  it("ネストと直列JOINが両方あっても組み立てられる") {
    val subject = youseibox.JsonObject(
      RootTableDefinition(
        "artist",
        LeafOneToOneTableDefinition(
          "artist_kana",
          "artist.id = artist_kana.artist_id"
        ).some
      ).some,
      Map[String, JsonValue](
        "name" -> JsonString("artist.name"),
        "kana" -> JsonString("artist_kana.kana"),
        "musics" -> JsonArray(
          LeafOneToManyTableDefinition(
            "music",
            "artist.id = music.artist_id",
            "artist.id"
          ).some,
          JsonString("music.name")
        )
      )
    )
    SQL(subject.toSql.selectMain).map(_.toMap).list.apply() |> asJsonObj should equal(
      List(Map("id" -> 1, "name" -> "水樹奈々",
        "json" -> Json(
          "name" := "水樹奈々",
          "kana" := "みずきなな",
          "musics" := Json.array(jString("深愛"), jString("innocent starter"))
        )
      ))
    )
  }

  it("ネスト内からネスト外のカラムを参照できる") {
    val subject = youseibox.JsonObject(
      RootTableDefinition(
        "artist"
      ).some,
      Map[String, JsonValue](
        "name" -> JsonString("artist.name"),
        "musics" -> JsonArray(
          LeafOneToManyTableDefinition(
            "music",
            "artist.id = music.artist_id",
            "artist.id"
          ).some,
          JsonInt("artist.id")
        )
      )
    )
    SQL(subject.toSql.selectMain).map(_.toMap).list.apply() |> asJsonObj should equal(
      List(Map("id" -> 1, "name" -> "水樹奈々",
        "json" -> Json(
          "name" := "水樹奈々",
          "musics" := Json.array(jNumber(1), jNumber(1))
        )
      ))
    )
  }

  it("jsonオブジェクトが配列の中だとしても組み立てられる") {
    val subject = youseibox.JsonObject(
      RootTableDefinition(
        "artist"
      ).some,
      Map[String, JsonValue](
        "name" -> JsonString("artist.name"),
        "musics" -> JsonArray(
          LeafOneToManyTableDefinition(
            "music",
            "artist.id = music.artist_id",
            "artist.id"
          ).some,
          youseibox.JsonObject(None, Map[String, JsonValue]("name" -> JsonString("music.name")))
        )
      )
    )
    SQL(subject.toSql.selectMain).map(_.toMap).list.apply() |> asJsonObj should equal(
      List(Map("id" -> 1, "name" -> "水樹奈々",
        "json" -> Json(
          "name" := "水樹奈々",
          "musics" := Json.array(Json("name" := "深愛"), Json("name" := "innocent starter"))
        )
      ))
    )
  }

  it("ネストが二重でも組み立てられる") {
    val subject = youseibox.JsonObject(
      RootTableDefinition(
        "artist"
      ).some,
      Map[String, JsonValue](
        "name" -> JsonString("artist.name"),
        "musics" -> JsonArray(
          LeafOneToManyTableDefinition(
            "music",
            "artist.id = music.artist_id",
            "artist.id"
          ).some,
          youseibox.JsonObject(
            LeafOneToManyTableDefinition(
              "music",
              "artist.id = music.artist_id",
              "artist.id"
            ).some,
            Map[String, JsonValue](
              "name" -> JsonString("music.name"),
              "contents" -> youseibox.JsonObject(
                LeafOneToManyTableDefinition(
                  "content",
                  "music.id = content.music_id", // ここのjoinRuleが
                  "music.id" // 外のcontentsという名前に引きづられるのいや
                ).some,
                Map[String, JsonValue](
                  "name" -> JsonString("content.name")
                )
              )
            )
          )
        )
      )
    )
    SQL(subject.toSql.selectMain).map(_.toMap).list.apply() |> asJsonObj should equal(
      List(Map("id" -> 1, "name" -> "水樹奈々",
        "json" -> Json(
          "name" := "水樹奈々",
          "musics" := Json.array(jString("深愛"), jString("innocent starter"))
        )
      ))
    )
  }

  it ("Optionで型を指定しているようなデータ構造はきっちりNoneを渡した時のテストも書いておくこと") {
    pending
  }
}
