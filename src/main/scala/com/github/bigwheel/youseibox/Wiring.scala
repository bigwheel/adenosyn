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
  val chain: Option[LeafOneToOneTableDefinition] = None
) extends LeafTableDefinition {
  val relation = OneToMany
}

trait JsonValue

case class JsonString(val refer: String) extends JsonValue

case class JsonObject(
  td: Option[TableDefinition],
  properties: Map[String, JsonValue]
) extends JsonValue

class Wiring {
  def forJsonString(js: JsonString): String = js.refer

  def forJsonObject(bluePrint: JsonObject): String = {
    val tableDefinition = bluePrint.td.get
    val properties = bluePrint.properties.map { p =>
      s""" '"${p._1}":"', ${p._2.asInstanceOf[JsonString].refer}, '"', """
    }.mkString("")
    s"""
       |SELECT
       |  ${tableDefinition.name}.*,
       |  CONCAT(
       |    '{',
       |    $properties
       |    '}'
       |  ) AS json
       |FROM
       |  ${tableDefinition.name}
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
          "artist.id = music.artist_id"
        ).some,
        Map[String, JsonValue](
          "name" -> JsonString("music.name"),
          "contents" -> JsonObject(
            LeafOneToManyTableDefinition(
              "content",
              "music.id = content.music_id"
            ).some,
            Map[String, JsonValue](
              "name" -> JsonString("content.name")
            )
          )
        )
      )
    )
  )

  println(forJsonObject(bluePrint.properties("musics").asInstanceOf[JsonObject].properties("contents").
    asInstanceOf[JsonObject]))

  //println(forJsonObject(bluePrint.properties("musics").asInstanceOf[JsonObject]))
}
