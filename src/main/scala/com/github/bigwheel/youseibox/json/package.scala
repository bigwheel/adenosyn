package com.github.bigwheel.youseibox

import com.github.bigwheel.youseibox.table._
import scala.util.Random
import scalaz.Scalaz._

package object json {

  trait JsValue {
    def toSql: SqlFragment
  }

  case class JsString(val tableName: String, val columnName: String) extends JsValue {
    def toSql = SqlFragment(s""" '"', $tableName.$columnName, '"' """)
  }

  case class JsInt(val tableName: String, val columnName: String) extends JsValue {
    def toSql = SqlFragment(tableName + "." + columnName)
  }

  case class JsArray(
    td: Option[_1toNTable],
    value: JsValue
  ) extends JsValue {
    lazy val toSql = {
      td match {
        case Some(tableDefinition) =>
          val temporaryViewName = Random.alphanumeric.take(10).mkString
          val preProcess = s"""
            |CREATE VIEW $temporaryViewName AS
            |SELECT
            |  ${tableDefinition.name}.${tableDefinition.joinColumnName},
            |  CONCAT('[', GROUP_CONCAT(${value.toSql.selectMain} SEPARATOR ','), ']') AS names
            |FROM ${tableDefinition.name}
            |${value.toSql.joinFragment.getOrElse("")}
            |GROUP BY ${tableDefinition.name}.${tableDefinition.joinColumnName}
            """.stripMargin
          val postProcess = s"DROP VIEW if exists $temporaryViewName"
          SqlFragment(
            value.toSql.preProcess :+ preProcess,
            s"$temporaryViewName.names",
            s"""
               |JOIN
               |  $temporaryViewName
               |ON
               |  ${tableDefinition.parentColumnName} = $temporaryViewName.${tableDefinition.joinColumnName}
               |""".stripMargin.some,
            postProcess +: value.toSql.postProcess
          )
        case None =>
        // TODO: Noneの場合のコードも書け
          throw new RuntimeException()
      }
    }
  }

  case class JsObject(
    tdo: Option[Table],
    properties: Map[String, JsValue]
  ) extends JsValue {
    def toSql = {
      val jsonObjectColumn = {
        val ps = properties.map { case (k, v) =>
          s""" '"$k":', ${v.toSql.selectMain}, """
        }.mkString("',',\n")
        s"CONCAT( '{', $ps '}' ) "
      }
      val joinFragments = properties.values.flatMap(_.toSql.joinFragment).mkString("\n")

      val (selectMain, joinFragment) = tdo match {
        case Some(tableDefinition: RootTable) =>
          val sm = s"SELECT $jsonObjectColumn AS json FROM ${tableDefinition.name} " +
            tableDefinition.getJoinStrings + joinFragments
          (sm, None)
        case Some(_) =>
          throw new IllegalStateException("")
        case None =>
          // TODO ここscalazの元の概念(モナドかも？)を使えばこんな無様なコードにはならない
          (jsonObjectColumn, joinFragments.some)
      }

      SqlFragment(
        properties.values.flatMap(_.toSql.preProcess).toSeq,
        selectMain,
        joinFragment,
        properties.values.flatMap(_.toSql.postProcess).toSeq
      )

    }
  }

}
