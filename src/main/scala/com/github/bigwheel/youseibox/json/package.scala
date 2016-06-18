package com.github.bigwheel.youseibox

import com.github.bigwheel.youseibox.table.Dot
import com.github.bigwheel.youseibox.table.JoinDefinition
import com.github.bigwheel.youseibox.table.Table

// あとでstructureとかそのままのjsonじゃなくてjsonの構造を定義するものだという意味を名前にきちんと込める
package object json {

  trait JsValue

  case class JsObject(
    tableOption: Option[Dot[Table, JoinDefinition]],
    properties: Map[String, JsValue]
  ) extends JsValue

  case class JsString(tableName: String, columnName: String) extends JsValue
  case class JsInt(tableName: String, columnName: String) extends JsValue

  /*trait OldJsValue {
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
  }*/

}
