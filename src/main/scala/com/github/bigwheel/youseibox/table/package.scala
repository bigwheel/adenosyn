package com.github.bigwheel.youseibox

import argonaut.Argonaut._
import argonaut._
import com.github.bigwheel.youseibox.json._
import scalaz.Scalaz._

package object table {

  case class Dot[D, L](value: D, lines: Line[L, D]*)
  case class Line[L, D](value: L, dot: Dot[D, L])

  class Table(val name: String, columnNameAndTypeMap: Map[String, String]) {
    val columns: Set[Column] = columnNameAndTypeMap.map(c => new Column(c._1, this, c._2)).toSet
    def getColumn(name: String): Column = columns.find(_.name === name).get
  }

  class Column(val name: String, val table: Table, val typeName: String) {
    def toSql = table.name + "." + name
  }

  case class JoinDefinition(
    columnOfParentTable: Column,
    _1toNRelation: Boolean,
    columnOfChildTable: Column) {
    def toSql(newChildTableName: String) = {
      val newChildColumnName = new FullColumnInfo(columnOfChildTable).nowColumnName
      s"ON ${columnOfParentTable.toSql} = $newChildTableName.$newChildColumnName"
    }
    def groupedBy(newTableName: String) = if (_1toNRelation)
      s" GROUP BY ${new FullColumnInfo(columnOfChildTable).updateTableName(newTableName).nowColumnName}"
    else
      ""
  }

  type DotTable = Dot[Table, JoinDefinition]
  type LineJoinDefinition = Line[JoinDefinition, Table]
  // typeではcompanion objectのエイリアスは作られないらしい。しょうがないから手作り
  object DotTable {
    def apply(value: Table, lines: LineJoinDefinition*): DotTable =
      Dot.apply[Table, JoinDefinition](value, lines: _*)
  }
  object LineJoinDefinition {
    def apply(value: JoinDefinition, dot: DotTable): LineJoinDefinition =
      Line.apply[JoinDefinition, Table](value, dot)
    def unapply(arg: LineJoinDefinition): Option[(JoinDefinition, DotTable)] =
      Line.unapply[JoinDefinition, Table](arg)
  }

  object FullColumnInfo {
    implicit class RichFullColumnInfoSet(fciSet: Set[FullColumnInfo]) {
      def getSelectSqlBody: String = fciSet.map(_.toColumnDefinition).mkString(", ")
    }
  }

  private class FullColumnInfo(val columnExpression: String, val nowColumnName: String, val originalColumn: Column) {
    def this(column: Column) = this(s"${column.toSql}",
      s"${column.table.name}__${column.name}__${column.typeName}", column)

    val toColumnDefinition = s"$columnExpression AS $nowColumnName"

    def bindUp(nestLevel: Int): FullColumnInfo = new FullColumnInfo(
      s"GROUP_CONCAT(${this.columnExpression} SEPARATOR ',${nestLevel + 1}')",
      this.nowColumnName + "s",
      this.originalColumn
    )

    def updateTableName(newTableName: String): FullColumnInfo =
      new FullColumnInfo(s"$newTableName.${this.nowColumnName}", this.nowColumnName, this.originalColumn)
  }

  // DotTableからQueryStringを作る
  def toSqlFromDot(dot: DotTable): String = toSqlFromDot(dot, 0)._1
  private def toSqlFromDot(dot: DotTable, nestLevel: Int): (String, Set[FullColumnInfo]) = {
    val table = dot.value
    val children = dot.lines.zipWithIndex.map { case (line, i) => toSqlFromLine(line, s"_$i", nestLevel) }
    val allFcis = table.columns.map { new FullColumnInfo(_) } ++ children.flatMap(_._2)
    val sql = s"SELECT ${allFcis.getSelectSqlBody} FROM ${table.name}"

    ((sql +: children.map(_._1)).mkString(" "), allFcis)
  }
  private def toSqlFromLine(line: LineJoinDefinition, newChildTableName: String, nestLevel: Int):
  (String, Set[FullColumnInfo]) = {
    val joinDefinition = line.value
    val plusLevel = if (joinDefinition._1toNRelation) 1 else 0
    val (sql, oldFcis) = toSqlFromDot(line.dot, nestLevel + plusLevel)
    val fcis = oldFcis.map(_.updateTableName("A"))
    val newFcis = if (joinDefinition._1toNRelation) {
      fcis.map { fci =>
        if (fci.originalColumn == joinDefinition.columnOfChildTable)
          fci
        else
          fci.bindUp(nestLevel)
      }
    } else fcis
    val newNewFcis = newFcis.map(_.updateTableName(newChildTableName))
    val shallowNestSql = s"SELECT ${newFcis.getSelectSqlBody} FROM ( $sql ) AS A ${joinDefinition.groupedBy("A")}"
    (s"JOIN ( $shallowNestSql ) AS $newChildTableName ${joinDefinition.toSql(newChildTableName)}", newNewFcis)
  }

  def toTableStructure(jsValue: JsValue): DotTable = {
    def a(jsValue: JsValue): Seq[LineJoinDefinition] = jsValue match {
      case JsObject(Some(line), b) =>
        val c = b.values.flatMap(a)
        val d: Seq[LineJoinDefinition] = line.dot.lines
        val e = (c ++ d).toSeq
        Seq(LineJoinDefinition(line.value, DotTable(line.dot.value, e: _*)))
      case JsArray(Some(line), b) =>
        val c = a(b)
        val d: Seq[LineJoinDefinition] = line.dot.lines
        val e = c ++ d
        Seq(LineJoinDefinition(line.value, DotTable(line.dot.value, e: _*)))
      case JsObject(None, b) =>
        val c = b.values.flatMap(a)
        c.toSeq
      // TDOO: arrayでtable definitonが空のテストケースを作れ、それでこの下に追加しろ
      case _ => Seq.empty
    }
    a(jsValue).head.dot
  }

  case class ParsedColumn(tableName: String, columnName: String, dimention: Int, value: Any) {
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

  def structureSqlResult(sqlResult: List[Map[String, Any]]): List[List[ParsedColumn]] = {
    val a = for (row <- sqlResult) yield {
      for ((columnName, value) <- row) yield {
        val result = """\A(.+)__(.+?)__(.+?)(s*)\Z""".r("tableName", "columnName", "type", "dimention").
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
        ParsedColumn(result.group("tableName"), result.group("columnName"), dim, drill(0, value.toString))
      }
    }
    a.map(_.toList)
  }

  def toJsonObj(parsedColumnss: List[List[ParsedColumn]] , jsonStructure: JsValue): List[Json] = {
    def f(row: List[ParsedColumn], jsonTree: JsValue): Json = {
      def getColumn(tableName: String, columnName: String): ParsedColumn =
        row.find(c => c.tableName == tableName && c.columnName == columnName).get

      jsonTree match {
        case JsObject(_, properties) =>
          Json(properties.map { case (k, v) => k := f(row, v) }.toSeq: _*)
        case JsArray(_, jsValue) =>
          val arrayLengths = row.flatMap(_.length).distinct
          require(arrayLengths.length == 1)
          (0 until arrayLengths.head).map { index =>
            f(row.map(_.drill(index)), jsValue)
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
