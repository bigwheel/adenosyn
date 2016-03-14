package com.github.bigwheel.youseibox

import scala.util.Random
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
  val childColumnForJoin: String,
  val parentColumnForGroupBy: String,
  val chain: Option[LeafOneToOneTableDefinition] = None
) extends LeafTableDefinition {
  val relation = OneToMany
}

case class SqlFragment(
  preProcess: Seq[String],
  selectMain: String,
  joinFragment: Option[String],
  postProcess: Seq[String]
)

object SqlFragment {
  def apply(selectMain: String, joinFragment: String): SqlFragment =
    this.apply(Nil, selectMain, Some(joinFragment), Nil)

  def apply(selectMain: String): SqlFragment =
    this.apply(Nil, selectMain, None, Nil)
}

trait JsonValue {
  def toSql: SqlFragment
}

case class JsonString(val columnName: String) extends JsonValue {
  def toSql = SqlFragment(s"""'"', $columnName, '"'""")
}

case class JsonInt(val columnName: String) extends JsonValue {
  def toSql = SqlFragment(columnName)
}

case class JsonArray(
  td: Option[TableDefinition],
  value: JsonValue
) extends JsonValue {
  lazy val toSql = {
    // TODO: Noneの場合のコードも書け
    val tableDefinition = td.get.asInstanceOf[LeafOneToManyTableDefinition]
    val temporaryViewName = Random.alphanumeric.take(10).mkString
    val preProcess = s"""
       |CREATE VIEW $temporaryViewName AS
       |SELECT
       |  ${tableDefinition.name}.${tableDefinition.childColumnForJoin},
       |  CONCAT('[', GROUP_CONCAT(${value.toSql.selectMain} SEPARATOR ','), ']') AS names
       |FROM ${tableDefinition.name}
       |${value.toSql.joinFragment.getOrElse("")}
       |GROUP BY ${tableDefinition.name}.${tableDefinition.childColumnForJoin}
     """.stripMargin
    val postProcess = s"DROP VIEW if exists $temporaryViewName"
    SqlFragment(
      value.toSql.preProcess :+ preProcess,
      s"""$temporaryViewName.names""",
      s"""
         |JOIN
         |  $temporaryViewName
         |ON
         |  ${tableDefinition.parentColumnForGroupBy} = $temporaryViewName.${tableDefinition.childColumnForJoin}
         |""".stripMargin.some,
      postProcess +: value.toSql.postProcess
    )
  }
}

case class JsonObject(
  tdo: Option[TableDefinition],
  properties: Map[String, JsonValue]
) extends JsonValue {
  def toSql = {
    tdo match {
      case Some(tableDefinition: LeafTableDefinition) =>
        throw new IllegalStateException("")
      case Some(tableDefinition: RootTableDefinition) =>
        val joinStringForDirectBoundedTables = tableDefinition.chain.map { innerTd =>
          s"""
             |JOIN
             |  ${innerTd.name}
             |ON
             |  ${innerTd.joinRule}
             |""".stripMargin
        }.getOrElse("")
        // TODO ここscalazの元の概念(モナドかも？)を使えばこんな無様なコードにはならない
        val tableName = tableDefinition.name
        val p = properties.map { case (k, v) =>
          s"""'"$k":', ${v.toSql.selectMain},"""
        }.mkString("',',")
        val joinStringForChildTables = properties.values.flatMap(_.toSql.joinFragment).mkString("\n")
        SqlFragment(
          properties.values.flatMap(_.toSql.preProcess).toSeq,
          s"""
            |SELECT
            |  $tableName.*,
            |  CONCAT(
            |      '{',
            |      $p
            |      '}'
            |  ) AS json
            |FROM
            |  $tableName
            |""".stripMargin + joinStringForDirectBoundedTables +
          joinStringForChildTables,
          Some(""),
          properties.values.flatMap(_.toSql.postProcess).toSeq
        )
      case None =>
        // 上と重複コードあり。気を見て統合する
        val p = properties.map { case (k, v) =>
          s"""'"$k":', ${v.toSql.selectMain},"""
        }.mkString("',',")
        val joins = properties.values.flatMap(_.toSql.joinFragment).mkString("\n")
        SqlFragment(
          properties.values.flatMap(_.toSql.preProcess).toSeq,
          s"""
             |  CONCAT(
             |      '{',
             |      $p
             |      '}'
             |  )
             |""".stripMargin
          , joins.some,
          properties.values.flatMap(_.toSql.postProcess).toSeq
        )
    }
  }
}

class Wiring {

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
          "artist_id",
          "artist.id"
        ).some,
        Map[String, JsonValue](
          "name" -> JsonString("music.name"),
          "contents" -> JsonObject(
            LeafOneToManyTableDefinition(
              "content",
              "music_id",
              "music.id"
            ).some,
            Map[String, JsonValue](
              "name" -> JsonString("content.name")
            )
          )
        )
      )
    )
  )

  //val musics = bluePrint.properties("musics").asInstanceOf[JsonObject]
  //println(forJsonObject("contents", musics.properties("contents").asInstanceOf[JsonObject]))

  //println(forJsonObject(bluePrint.properties("musics").asInstanceOf[JsonObject]))
}
