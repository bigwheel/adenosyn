package com.github.bigwheel.youseibox

import scalaz.Scalaz._

sealed abstract class Relation
object OneToOne extends Relation
object OneToMany extends Relation

trait TableDefinition {
  val name: String
  val chain: Option[LeafOneToOneTableDefinition]
}

case class RootTableDefinition(
  val name: String,
  val chain: Option[LeafOneToOneTableDefinition] = None
) extends TableDefinition

trait LeafTableDefinition extends TableDefinition {
  val joinRule: String // 親との接続ルール
  val relation: Relation
}

case class LeafOneToOneTableDefinition(
  val name: String,
  val joinRule: String,
  val chain: Option[LeafOneToOneTableDefinition] = None
) extends LeafTableDefinition {
  val relation = OneToOne
}

case class LeafOneToManyTableDefinition(
  val name: String,
  val joinRule: String,
  val parentGroupByColumn: String,
  val chain: Option[LeafOneToOneTableDefinition] = None
) extends LeafTableDefinition {
  val relation = OneToMany
}

trait JsonValue {
  def toSql: String
}

case class JsonString(val columnName: String) extends JsonValue {
  def toSql = s"""'"', $columnName, '"'"""
}

case class JsonInt(val columnName: String) extends JsonValue {
  def toSql = columnName
}

case class JsonObject(
  td: Option[TableDefinition],
  properties: Map[String, JsonValue]
) extends JsonValue {
  def toSql = ""
}

object Wiring {
  def renderSql(jo: JsonObject): String = {
    val tableName = jo.td.get.asInstanceOf[RootTableDefinition].name
    val properties = jo.properties.map { case (k, v) =>
      s"""'"$k":', ${v.toSql},"""
    }.mkString("',',")
    s"""
      |SELECT
      |  $tableName.*,
      |  CONCAT(
      |      '{',
      |      $properties
      |      '}'
      |  ) AS json
      |FROM
      |  $tableName
      |""".stripMargin
  }
}

class Wiring {
  def forJsonString(js: JsonString): String = js.toSql

  def forJsonObject(key: String, bluePrint: JsonObject): String = {
    val tableDefinition = bluePrint.td.get.asInstanceOf[LeafTableDefinition]
    val tail = tableDefinition match {
      case l: LeafOneToManyTableDefinition => "GROUP BY\n  " + l.parentGroupByColumn
      case _ => ""
    }
    val properties = bluePrint.properties.map { p =>
      p._2 match {
        case js: JsonString => s""" '"${p._1}":', ${js.toSql}, """
        case jo: JsonObject => s""" '"${p._1}":[', GROUP_CONCAT(${p._1}.json SEPARATOR ','), ']',"""
      }
    }.mkString("")
    s"""
       |(
       |  SELECT
       |    ${tableDefinition.name}.*,
       |    CONCAT(
       |      '{',
       |      $properties
       |      '}'
       |    ) AS json
       |  FROM
       |    ${tableDefinition.name}
       |) AS $key
       |ON
       |  ${tableDefinition.joinRule}
       |$tail
    """.stripMargin
  }

  val bluePrint = JsonObject(
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
      "musics" -> JsonObject(
        LeafOneToManyTableDefinition(
          "music",
          "artist.id = music.artist_id",
          "artist.id"
        ).some,
        Map[String, JsonValue](
          "name" -> JsonString("music.name"),
          "contents" -> JsonObject(
            LeafOneToManyTableDefinition(
              "content",
              "music.id = contents.music_id", // ここのjoinRuleが
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

  val musics = bluePrint.properties("musics").asInstanceOf[JsonObject]
  println(forJsonObject("contents", musics.properties("contents").asInstanceOf[JsonObject]))

  //println(forJsonObject(bluePrint.properties("musics").asInstanceOf[JsonObject]))
}
