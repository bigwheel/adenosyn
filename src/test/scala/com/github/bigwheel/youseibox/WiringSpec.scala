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
    s"jdbc:mysql://$ipAddress/youseibox_test?characterEncoding=UTF-8&useSSL=false", "youseibox", "youseibox")
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

  /*it("開発環境VMが動いている") { // テスト毎に時間が数秒余計にかかるのでコメントアウト
    val result = Process("otto dev vagrant status").!!
    result.split("\n")(3) should
      equal("default                   running (virtualbox)")
  }*/

  it("開発環境のIPアドレスが正しく取得できる") {
    ipAddress should fullyMatch regex """\A\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}\Z"""
  }

  val artistTable = new Table("artist", ("id", "Int"), ("name", "String"))
  val artistKanaTable = new Table("artist_kana", ("artist_id", "Int"), ("kana", "String"))
  val musicTable = new Table("music", ("id", "Int"), ("artist_id", "Int"), ("name", "String"))
  val contentTable = new Table("content", ("id", "Int"), ("music_id", "Int"), ("name", "String"))

  it("1テーブルのSQLが出力できる") {
    val tableTree = DotTable(artistTable)
    val sqlResult = SQL(table.toSqlFromDot(tableTree)._1).map(_.toMap).list.apply()
    sqlResult should equal(List(Map(
      "artist__id__Int" -> 1,
      "artist__name__String" -> "水樹奈々"
    )))
  }

  it("1対1の関係のJOINができる") {
    val tableTree = DotTable(
      artistTable,
      LineJoinDefinition(
        JoinDefinition(artistTable.getColumn("id"), false, artistKanaTable.getColumn("artist_id")),
        DotTable(artistKanaTable)
      )
    )
    val sqlResult = SQL(table.toSqlFromDot(tableTree)._1).map(_.toMap).list.apply()
    sqlResult should equal(List(Map(
      "artist__id__Int" -> 1,
      "artist__name__String" -> "水樹奈々",
      "artist_kana__artist_id__Int" -> 1,
      "artist_kana__kana__String" -> "みずきなな"
    )))
  }

  it("1対Nの関係のJOINができる") {
    val tableTree = DotTable(
      artistTable,
      LineJoinDefinition(
        JoinDefinition(artistTable.getColumn("id"), true, musicTable.getColumn("artist_id")),
        DotTable(musicTable)
      )
    )
    val sqlResult = SQL(table.toSqlFromDot(tableTree)._1).map(_.toMap).list.apply()
    sqlResult should equal(List(Map(
      "artist__id__Int" -> 1,
      "artist__name__String" -> "水樹奈々",
      "music__id__Ints" -> "11,112",
      "music__artist_id__Int" -> 1,
      "music__name__Strings" -> "深愛,1innocent starter"
    )))
  }

  it("1対Nの関係をネストしてもJOINができる") {
    val tableTree = DotTable(
      artistTable,
      LineJoinDefinition(
        JoinDefinition(artistTable.getColumn("id"), true, musicTable.getColumn("artist_id")),
        DotTable(
          musicTable,
          LineJoinDefinition(
            JoinDefinition(musicTable.getColumn("id"), true, contentTable.getColumn("music_id")),
            DotTable(contentTable)
          )
        )
      )
    )
    val sqlResult = SQL(table.toSqlFromDot(tableTree)._1).map(_.toMap).list.apply()
    sqlResult should equal(List(Map(
      "artist__id__Int" -> 1,
      "artist__name__String" -> "水樹奈々",
      "music__id__Ints" -> "11,112",
      "music__artist_id__Int" -> 1,
      "music__name__Strings" -> "深愛,1innocent starter",
      "content__id__Intss" -> "111,2112,1121",
      "content__music_id__Ints" -> "11,112",
      "content__name__Stringss" -> "深愛 - ショートVer.,2深愛 - ロングVer.,1innocent starter(inst)"
    )))
  }

  case class TestCase(title: String, input: JsValue, expected: List[Json])
  val tests = Seq[TestCase](
    TestCase(
      "最も単純なjsonオブジェクトを組み立てられる",
      JsObject(
        LineJoinDefinition(null, DotTable(artistTable)).some,
        Map[String, JsValue](
          "name" -> JsString("artist", "name")
        )
      ),
      List(Json("name" := "水樹奈々"))
    ),
    TestCase(
      "複数プロパティのjsonオブジェクトを組み立てられる",
      JsObject(
        LineJoinDefinition(null, DotTable(artistTable)).some,
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
        LineJoinDefinition(
          null,
          DotTable(
            artistTable,
            LineJoinDefinition(
              JoinDefinition(artistTable.getColumn("id"), false, artistKanaTable.getColumn("artist_id")),
              DotTable(artistKanaTable)
            )
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
        LineJoinDefinition(null, DotTable(artistTable)).some,
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
    ),
    TestCase(
      "ネストと直列JOINが両方あっても組み立てられる",
      JsObject(
        LineJoinDefinition(
          null,
          DotTable(
            artistTable,
            LineJoinDefinition(
              JoinDefinition(artistTable.getColumn("id"), false, artistKanaTable.getColumn("artist_id")),
              DotTable(artistKanaTable)
            )
          )
        ).some,
        Map[String, JsValue](
          "name" -> JsString("artist", "name"),
          "kana" -> JsString("artist_kana", "kana"),
          "musics" -> JsArray(
            LineJoinDefinition(
              JoinDefinition(artistTable.getColumn("id"), true, musicTable.getColumn("artist_id")),
              DotTable(musicTable)
            ).some,
            JsString("music", "name")
          )
        )
      ),
      List(Json(
        "name" := "水樹奈々",
        "kana" := "みずきなな",
        "musics" := Json.array(jString("深愛"), jString("innocent starter"))
      ))
    ),
    TestCase(
      "ネスト内からネスト外のカラムを参照できる",
      JsObject(
        LineJoinDefinition(null, DotTable(artistTable)).some,
        Map[String, JsValue](
          "name" -> JsString("artist", "name"),
          "musics" -> JsArray(
            LineJoinDefinition(
              JoinDefinition(artistTable.getColumn("id"), true, musicTable.getColumn("artist_id")),
              DotTable(musicTable)
            ).some,
            JsInt("artist", "id")
          )
        )
      ),
      List(Json(
        "name" := "水樹奈々",
        "musics" := Json.array(jNumber(1), jNumber(1))
      ))
    ),
    TestCase(
      "jsonオブジェクトが配列の中だとしても組み立てられる",
      JsObject(
        LineJoinDefinition(null, DotTable(artistTable)).some,
        Map[String, JsValue](
          "name" -> JsString("artist", "name"),
          "musics" -> JsArray(
            LineJoinDefinition(
              JoinDefinition(artistTable.getColumn("id"), true, musicTable.getColumn("artist_id")),
              DotTable(musicTable)
            ).some,
            JsObject(None, Map[String, JsValue]("name" -> JsString("music", "name")))
          )
        )
      ),
      List(Json(
        "name" := "水樹奈々",
        "musics" := Json.array(Json("name" := "深愛"), Json("name" := "innocent starter"))
      ))
    ),
    TestCase(
      "ネストが二重でも組み立てられる",
      JsObject(
        LineJoinDefinition(null, DotTable(artistTable)).some,
        Map[String, JsValue](
          "name" -> JsString("artist", "name"),
          "musics" -> JsArray(
            LineJoinDefinition(
              JoinDefinition(artistTable.getColumn("id"), true, musicTable.getColumn("artist_id")),
              DotTable(musicTable)
            ).some,
            JsObject(
              None,
              Map[String, JsValue](
                "name" -> JsString("music", "name"),
                "contents" -> JsArray(
                  LineJoinDefinition(
                    JoinDefinition(musicTable.getColumn("id"), true, contentTable.getColumn("music_id")),
                    DotTable(contentTable)
                  ).some,
                  JsObject(
                    None,
                    Map[String, JsValue](
                      "name" -> JsString("content", "name")
                    )
                  )
                )
              )
            )
          )
        )
      ),
      List(Json(
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
      ))
    )
  )

  for (test <- tests) {
    it(test.title) {
      val tableTree: DotTable = toTableStructure(test.input)
      val sqlResult = SQL(table.toSqlFromDot(tableTree)._1).map(_.toMap).list.apply()
      //println(sqlResult)
      val parsedColumnss = structureSqlResult(sqlResult)
      //println(parsedColumnss)
      def sqlResultToJson: List[Json] = toJsonObj(parsedColumnss, test.input)
      sqlResultToJson should equal(test.expected)
    }
  }

  // https://dev.mysql.com/doc/refman/5.6/ja/group-by-functions.html#function_group-concat
  // TODO: group_concatの最大長について修正するようにしろ、ドキュメントでもいいけど

  /*
      TestCase(
      "jsonオブジェクトが配列の中だとしても組み立てられる",
      JsObject(
        LineJoinDefinition(null, DotTable(artistTable)).some,
        Map[String, JsValue](
          "name" -> JsString("artist", "name"),
          "musics" -> JsArray(
            LineJoinDefinition(
              JoinDefinition(artistTable.getColumn("id"), false, artistKanaTable.getColumn("artist_id")),
              DotTable(musicTable)
            ).some,
            JsObject(None, Map[String, JsValue]("name" -> JsString("music", "name")))
          )
        )
      ),
      List(Json(
        "name" := "水樹奈々",
        "musics" := Json.array(Json("name" := "深愛"), Json("name" := "innocent starter"))
      ))
    )
こういうTableJoin構造が間違っている奴、処理中に間違っていると出すようにする
(sql吐いて実行したあとにエラーを出す)
あとjsArray使っているのにJsonDefinitionの2番めがtrueじゃない奴とかも事前チェックでエラーにする
)
   */
}
