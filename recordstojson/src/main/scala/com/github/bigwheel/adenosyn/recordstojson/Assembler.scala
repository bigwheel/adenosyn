package com.github.bigwheel.adenosyn.recordstojson

import argonaut.Argonaut._
import argonaut._
import com.github.bigwheel.adenosyn.recordstojson.dsl._
import scalaz.Scalaz._
import scalikejdbc.DBSession
import scalikejdbc.SQL

class Assembler(implicit session: DBSession) {

  def AssembleAll(jsValue: JsValue): List[Json] = {
    val queryString: SqlQuery = jsValue.toSql
    val sqlResult: List[Map[String, Any]] = SQL(queryString).map(_.toMap).list.apply()
    val parsedColumnss: List[List[ParsedColumn]] = structureSqlResult(sqlResult)
    toJsonObj(parsedColumnss, jsValue)
  }

  private[this] case class ParsedColumn(tableName: String, columnName: String, dimention: Int, value: Any) {
    // arrayじゃなくても結果返すので注意
    def drill(index: Int): ParsedColumn = value match {
      case a: Array[_] => new ParsedColumn(tableName, columnName, dimention - 1, a(index))
      case a => new ParsedColumn(tableName, columnName, dimention, a)
    }

    val length: Option[Int] = value match {
      case a: Array[_] => a.length.some
      case _ => none
    }
  }

  private[this] def structureSqlResult(sqlResult: List[Map[String, Any]]): List[List[ParsedColumn]] = {
    val a = for (row <- sqlResult) yield {
      for ((columnName, value) <- row) yield {
        val result =
          """\A(.+)__(.+?)__(.+?)(s*)\Z""".r("tableName", "columnName", "type", "dimention").
            findFirstMatchIn(columnName).get

        val dim = result.group("dimention").count(_ == 's')
        def drill(level: Int, str: String): Any = if (dim - level == 0) {
          result.group("type") match {
            case "Int" => str.toInt
            case "String" => str
          }
        } else {
          val splitted = str.asInstanceOf[String].split("," + (level + 1).toString)
          splitted.map(drill(level + 1, _))
        }
        ParsedColumn(result.group("tableName"),
          result.group("columnName"),
          dim,
          drill(0, value.toString))
      }
    }
    a.map(_.toList)
  }

  private[this] def toJsonObj(parsedColumnss: List[List[ParsedColumn]],
    jsonStructure: JsValue): List[Json] = {
    def f(row: List[ParsedColumn], jsonTree: JsValue): Json = {
      def getColumn(tableName: String, columnName: String): ParsedColumn =
        row.find(c => c.tableName == tableName && c.columnName == columnName).get

      jsonTree match {
        case JsObject(_, properties) =>
          Json(properties.map { case (k, v) => k := f(row, v) }.toSeq: _*)
        case JsArray(_, _jsValue) =>
          val arrayLengths = row.flatMap(_.length).distinct
          require(arrayLengths.length == 1)
          (0 until arrayLengths.head).map { index =>
            f(row.map(_.drill(index)), _jsValue)
          }.toList |> jArray.apply
        case JsString(tableName, columnName) =>
          jString(getColumn(tableName, columnName).value.asInstanceOf[String])
        case JsInt(tableName, columnName) =>
          jNumber(getColumn(tableName, columnName).value.asInstanceOf[Int])
      }
    }

    for (row <- parsedColumnss) yield f(row, jsonStructure)
  }

}
