package com.github.bigwheel.youseibox

import com.github.bigwheel.youseibox.table.OldTable
import com.github.bigwheel.youseibox.table._1toNTable

package object json {

  trait OldJsValue {
    def tableStructure: String
    def toSql: String
  }

  case class OldJsString(tableName: String, columnName: String) extends OldJsValue {
    def tableStructure = ""
    val toSql = tableName + "." + columnName
  }

  case class OldJsInt(tableName: String, columnName: String) extends OldJsValue {
    def tableStructure = ""
    val toSql = tableName + "." + columnName
  }

  case class OldJsArray(to: Option[_1toNTable], jsValue: OldJsValue) extends OldJsValue {
    def tableStructure = ???
    override def toSql: String = ""
  }

  case class OldJsObject(
    tableOption: Option[OldTable],
    properties: Map[String, OldJsValue]
  ) extends OldJsValue {
    def tableStructure = ???

    val toSql = {
      val selectBody = properties.map { case (name, value) =>
        val columnName = value match {
          case _: OldJsString => name + "_STRING"
          case _: OldJsInt => name + "_INT"
          case _: OldJsObject =>
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
