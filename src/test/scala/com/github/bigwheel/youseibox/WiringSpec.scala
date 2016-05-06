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
        "drop table if exists content",
        "drop table if exists tieup"
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
        """,
        """
        create table tieup (
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
        """INSERT INTO content(id, music_id, name) VALUES (121, 12, "innocent starter(inst)")""",
        """INSERT INTO tieup(id, music_id, name) VALUES (1101, 11, "White Album 2")""",
        """INSERT INTO tieup(id, music_id, name) VALUES (1201, 12, "魔法少女リリカルなのは")"""
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

  val artistTable = Table("artist", "id", "name")
  val artistKanaTable = Table("artist_kana", "artist_id", "kana")
  val musicTable = Table("music", "id", "artist_id", "name")
  val contentTable = Table("content", "id", "music_id", "name")

  it("1対1の関係のJOINができる") {
    val tableStructure = Dot[Table, JoinDefinition](
      artistTable,
      Line[JoinDefinition, Table](
        JoinDefinition("id", false, "artist_id"),
        Dot[Table, JoinDefinition](artistKanaTable)
      )
    )
    table.toSql(tableStructure) should
      equal("SELECT artist.id AS artist__id, artist.name AS artist__name, " +
        "artist_kana.artist_id AS artist_kana__artist_id, artist_kana.kana AS artist_kana__kana " +
        "FROM artist JOIN artist_kana ON artist.id = artist_kana.artist_id")
  }

  it("1対Nの関係のJOINができる") {
    val tableStructure = Dot[Table, JoinDefinition](
      artistTable,
      Line[JoinDefinition, Table](
        JoinDefinition("id", true, "artist_id"),
        Dot[Table, JoinDefinition](musicTable)
      )
    )
    table.toSql(tableStructure) should
      equal("SELECT artist.id AS artist__id, artist.name AS artist__name, " +
        "GROUP_CONCAT(music.id) AS music__ids, GROUP_CONCAT(music.artist_id) AS music__artist_ids, " +
        "GROUP_CONCAT(music.name) AS music__names " +
        "FROM artist JOIN music ON artist.id = music.artist_id GROUP BY artist.id")
  }

  // こういう感じ。
  val nomean = """SELECT
                | artist.id AS artist__id,
                | artist.name AS artist__name,
                | GROUP_CONCAT(A.music__id) AS music__ids,
                | GROUP_CONCAT(A.music__artist_id) AS music__artist_ids,
                | GROUP_CONCAT(content__ids) AS content__idss
                | FROM artist JOIN
                | (
                |   SELECT
                |     music.id AS music__id,
                |     music.artist_id AS music__artist_id,
                |     music.name AS music__name,
                |     GROUP_CONCAT(content.id) AS content__ids,
                |     GROUP_CONCAT(content.name) AS content__names
                |   FROM music JOIN content
                |   ON music.id = content.music_id GROUP BY music.id
                | ) AS A
                | ON artist.id = A.music__artist_id GROUP BY artist.id""".stripMargin.split("\n").map(_.trim).mkString(" ")

  case class TestCase(title: String, input: JsValue, expected: List[Json])
  val tests = Seq[TestCase](
    TestCase(
      "最も単純なjsonオブジェクトを組み立てられる",
      JsObject(
        OldTable("artist").some,
        Map[String, JsValue](
          "name" -> JsString("artist", "name")
        )
      ),
      List(Json("name" := "水樹奈々"))
    )/*,
    Test(
      "複数プロパティのjsonオブジェクトを組み立てられる",
      JsObject(
        Table("artist").some,
        Map[String, JsValue](
          "id" -> JsInt("artist", "id"),
          "name" -> JsString("artist", "name")
        )
      ),
      List(Json("id" := 1, "name" := "水樹奈々"))
    ),
    Test(
      "単純チェインのテーブルJOINでjsonオブジェクトを組み立てられる",
      JsObject(
        Table(
          "artist",
          ChainedTable(
            "artist_kana",
            "id",
            "artist_id"
          ).some
        ).some,
        Map[String, JsValue](
          "name" -> JsString("artist", "name"),
          "kana" -> JsString("artist_kana", "kana")
        )
      ),
      List(Json("name" := "水樹奈々", "kana" := "みずきなな"))
    ),
    Test(
      "単純にjsonオブジェクトがネストしていても組み立てられる",
      JsObject(
        Table(
          "artist"
        ).some,
        Map[String, JsValue](
          "name" -> JsString("artist", "name"),
          "nest" -> JsObject(
            None,
            Map[String, JsValue](
              "name" -> JsString("artist", "name")
            )
          )
        )
      ),
      List(Json(
        "name" := "水樹奈々",
        "musics" := Json.array(jString("深愛"), jString("innocent starter"))
      ))
    )*/
  )

  def sqlResultToJson(sqlResult: List[Map[String, Any]]): List[Json] = sqlResult.map { row =>
    val columnRegex = "^(.+)_([^_]+)$".r
    row.toSeq.map { case (columnRegex(trueColumnName, typeAnnotation), joinedValues: String) =>
      val value = typeAnnotation match {
        case "INT" => jNumber(joinedValues.toInt)
        case "STRING" => jString(joinedValues)
      }
      trueColumnName := value
    } |> Json.apply
  }

  for (test <- tests) {
    it(test.title) {
      val sqlResult = SQL(test.input.toSql).map(_.toMap).list.apply()
      sqlResult |> sqlResultToJson should equal(test.expected)
    }
  }

}
