package com.github.bigwheel.youseibox

import com.github.bigwheel.youseibox.table.Table

package object json {

  trait JsValue {
    def toSql: String
  }

  case class JsString(tableName: String, columnName: String) extends JsValue {
    val toSql = tableName + "." + columnName
  }

  case class JsInt(tableName: String, columnName: String) extends JsValue {
    val toSql = tableName + "." + columnName
  }

  abstract class JsArray extends JsValue

  case class JsObject(
    to: Option[Table],
    properties: Map[String, JsValue]
  ) extends JsValue {
    val selectBody = properties.map { a =>
      val columnName = a._2 match {
        case _: JsString => a._1 + "_STRING"
        case _: JsInt => a._1 + "_INT"
      }
      s"GROUP_CONCAT(${a._2.toSql})" + " AS " + columnName
    }.mkString(", ")
    val toSql = s"SELECT $selectBody FROM ${to.get.definition}"
  }

}
