package com.github.bigwheel.youseibox

import com.github.bigwheel.youseibox.table.Table
import com.github.bigwheel.youseibox.table._1toNTable

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

  case class JsArray(to: Option[_1toNTable], jsValue: JsValue) extends JsValue {
    override def toSql: String = ""
  }

  case class JsObject(
    tableOption: Option[Table],
    properties: Map[String, JsValue]
  ) extends JsValue {
    val selectBody = properties.map { case (name, value) =>
      val columnName = value match {
        case _: JsString => name + "_STRING"
        case _: JsInt => name + "_INT"
        case _ => ???
      }
      s"GROUP_CONCAT(${value.toSql})" + " AS " + columnName
    }.mkString(", ")
    val toSql = s"SELECT $selectBody FROM ${tableOption.get.definition}"
  }

}
