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

  /*it("開発環境VMが動いている") {
    val result = Process("otto dev vagrant status").!!
    result.split("\n")(3) should
      equal("default                   running (virtualbox)")
  }*/

  it("開発環境のIPアドレスが正しく取得できる") {
    ipAddress should fullyMatch regex """\A\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}\Z"""
  }

  val artistTable = new Table("artist", "id", "name")
  val artistKanaTable = new Table("artist_kana", "artist_id", "kana")
  val musicTable = new Table("music", "id", "artist_id", "name")
  val contentTable = new Table("content", "id", "music_id", "name")

  it("1テーブルのSQLが出力できる") {
    val tableTree = Dot[Table, JoinDefinition](artistTable)
    val sqlResult = SQL(table.toSqlFromDot(tableTree)._1).map(_.toMap).list.apply()
    sqlResult should equal(List(Map(
      "artist__id" -> 1,
      "artist__name" -> "水樹奈々"
    )))
  }

  it("1対1の関係のJOINができる") {
    val tableTree = Dot[Table, JoinDefinition](
      artistTable,
      Line[JoinDefinition, Table](
        JoinDefinition(artistTable.getColumn("id"), false, artistKanaTable.getColumn("artist_id")),
        Dot[Table, JoinDefinition](artistKanaTable)
      )
    )
    val sqlResult = SQL(table.toSqlFromDot(tableTree)._1).map(_.toMap).list.apply()
    sqlResult should equal(List(Map(
      "artist__id" -> 1,
      "artist__name" -> "水樹奈々",
      "artist_kana__artist_id" -> 1,
      "artist_kana__kana" -> "みずきなな"
    )))
  }

  it("1対Nの関係のJOINができる") {
    val tableTree = Dot[Table, JoinDefinition](
      artistTable,
      Line[JoinDefinition, Table](
        JoinDefinition(artistTable.getColumn("id"), true, musicTable.getColumn("artist_id")),
        Dot[Table, JoinDefinition](musicTable)
      )
    )
    val sqlResult = SQL(table.toSqlFromDot(tableTree)._1).map(_.toMap).list.apply()
    sqlResult should equal(List(Map(
      "music__ids" -> "11,12",
      "music__names" -> "深愛,innocent starter",
      "music__artist_ids" -> "1,1",
      "artist__name" -> "水樹奈々",
      "artist__id" -> 1
    )))
  }

  it("1対Nの関係をネストしてもJOINができる") {
    val tableStructure = Dot[Table, JoinDefinition](
      artistTable,
      Line[JoinDefinition, Table](
        JoinDefinition(artistTable.getColumn("id"), true, musicTable.getColumn("artist_id")),
        Dot[Table, JoinDefinition](
          musicTable,
          Line[JoinDefinition, Table](
            JoinDefinition(musicTable.getColumn("id"), true, contentTable.getColumn("music_id")),
            Dot[Table, JoinDefinition](contentTable)
          )
        )
      )
    )
    table.toSql(tableStructure)._1 should
      equal("""SELECT
              | GROUP_CONCAT(A.content__music_ids) AS content__music_idss,
              | GROUP_CONCAT(A.music__name) AS music__names,
              | artist.name AS artist__name,
              | GROUP_CONCAT(A.content__names) AS content__namess,
              | GROUP_CONCAT(A.music__artist_id) AS music__artist_ids,
              | artist.id AS artist__id,
              | GROUP_CONCAT(A.music__id) AS music__ids,
              | GROUP_CONCAT(A.content__ids) AS content__idss
              | FROM artist JOIN
              | (
              |   SELECT
              |     music.id AS music__id,
              |     music.artist_id AS music__artist_id,
              |     music.name AS music__name,
              |     GROUP_CONCAT(content.music_id) AS content__music_ids,
              |     GROUP_CONCAT(content.name) AS content__names,
              |     GROUP_CONCAT(content.id) AS content__ids
              |   FROM music JOIN content
              |   ON music.id = content.music_id GROUP BY music.id
              | ) AS A
              | ON artist.id = A.music__artist_id GROUP BY artist.id""".
        stripMargin.split("\n").map(_.trim).mkString(" "))
  }

  def toTableStructure(jsValue: JsValue): Dot[Table, JoinDefinition] = jsValue match {
    case JsObject(Some(dots), _) => dots
  }

  def toJsonObj(sqlResult: List[Map[String, Any]], jsonStructure: JsValue): List[Json] = {
    def f(row: Map[String, Any], jsonStructure: JsValue): Json = jsonStructure match {
      case JsObject(_, properties) =>
        val c = properties.map { property =>
          property._2 match {
            case a: JsString =>
              property._1 := row(a.tableName + "__" + a.columnName).asInstanceOf[String]
            case a: JsInt =>
              property._1 := row(a.tableName + "__" + a.columnName).asInstanceOf[Int]
            case a: JsObject =>
              property._1 := f(row, a)
          }
        }
        Json(c.toSeq: _*)
      case _ => ???
    }

    for (row <- sqlResult) yield f(row, jsonStructure)
  }

  case class TestCase(title: String, input: JsValue, expected: List[Json])
  val tests = Seq[TestCase](
    TestCase(
      "最も単純なjsonオブジェクトを組み立てられる",
      JsObject(
        Dot[Table, JoinDefinition](artistTable).some,
        Map[String, JsValue](
          "name" -> JsString("artist", "name")
        )
      ),
      List(Json("name" := "水樹奈々"))
    ),
    TestCase(
      "複数プロパティのjsonオブジェクトを組み立てられる",
      JsObject(
        Dot[Table, JoinDefinition](artistTable).some,
        Map[String, JsValue](
          "id" -> JsInt("artist", "id"),
          "name" -> JsString("artist", "name")
        )
      ),
      List(Json("id" := 1, "name" := "水樹奈々"))
    ),
    TestCase(
      "単純チェインのテーブルJOINでjsonオブジェクトを組み立てられる",
      JsObject(
        Dot[Table, JoinDefinition](
          artistTable,
          Line[JoinDefinition, Table](
            JoinDefinition(artistTable.getColumn("id"), false, artistKanaTable.getColumn("artist_id")),
            Dot[Table, JoinDefinition](artistKanaTable)
          )
        ).some,
        Map[String, JsValue](
          "name" -> JsString("artist", "name"),
          "kana" -> JsString("artist_kana", "kana")
        )
      ),
      List(Json("name" := "水樹奈々", "kana" := "みずきなな"))
    ),
    TestCase(
      "単純にjsonオブジェクトがネストしていても組み立てられる",
      JsObject(
        Dot[Table, JoinDefinition](artistTable).some,
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
        "nest" := Json("name" := "水樹奈々")
      ))
    )
  )

  for (test <- tests) {
    it(test.title) {
      val tableStructure: Dot[Table, JoinDefinition] = toTableStructure(test.input)
      val sqlResult = SQL(table.toSql(tableStructure)._1).map(_.toMap).list.apply()
      def sqlResultToJson: List[Json] = toJsonObj(sqlResult, test.input)
      sqlResultToJson should equal(test.expected)
    }
  }

}
