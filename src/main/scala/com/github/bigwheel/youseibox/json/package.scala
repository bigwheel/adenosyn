package com.github.bigwheel.youseibox

import com.github.bigwheel.youseibox.table.OldTable
import com.github.bigwheel.youseibox.table._1toNTable

package object json {

  trait JsValue {
    def tableStructure: String
    def toSql: String
  }

  case class JsString(tableName: String, columnName: String) extends JsValue {
    def tableStructure = ""
    val toSql = tableName + "." + columnName
  }

  case class JsInt(tableName: String, columnName: String) extends JsValue {
    def tableStructure = ""
    val toSql = tableName + "." + columnName
  }

  case class JsArray(to: Option[_1toNTable], jsValue: JsValue) extends JsValue {
    def tableStructure = ???
    override def toSql: String = ""
  }

  case class JsObject(
    tableOption: Option[OldTable],
    properties: Map[String, JsValue]
  ) extends JsValue {
    def tableStructure = ???

    val toSql = {
      val selectBody = properties.map { case (name, value) =>
        val columnName = value match {
          case _: JsString => name + "_STRING"
          case _: JsInt => name + "_INT"
          case _: JsObject =>
          case _ => ???
        }
        s"GROUP_CONCAT(${value.toSql})" + " AS " + columnName
      }.mkString(", ")

      tableOption match {
        case Some(table) => s"SELECT $selectBody FROM ${table.definition}"
        case None => selectBody
      }
    }
  }

}
