package com.github.bigwheel.youseibox

import com.github.bigwheel.youseibox.table.Table
import scala.util.Random
import scalaz.Scalaz._

package object json {

  trait JsValue {
    def toSql: SqlFragment
  }

  case class JsString(val columnName: String) extends JsValue {
    def toSql = SqlFragment(s"""'"', $columnName, '"'""")
  }

  case class JsInt(val columnName: String) extends JsValue {
    def toSql = SqlFragment(columnName)
  }

  case class JsArray(
    td: Option[Table],
    value: JsValue
  ) extends JsValue {
    lazy val toSql = {
      // TODO: Noneの場合のコードも書け
      val tableDefinition = td.get.asInstanceOf[LeafOneToManyTable]
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

  case class JsObject(
    tdo: Option[Table],
    properties: Map[String, JsValue]
  ) extends JsValue {
    def toSql = {
      tdo match {
        case Some(tableDefinition: RootTable) =>
          val joinStringForDirectBoundedTables = tableDefinition.chainTable.map { innerTd =>
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
        case Some(_) =>
          throw new IllegalStateException("")
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

}
