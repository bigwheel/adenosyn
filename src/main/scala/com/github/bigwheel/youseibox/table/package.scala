package com.github.bigwheel.youseibox

import argonaut.Argonaut._
import argonaut._
import com.github.bigwheel.youseibox.json._
import scalaz._
import scalaz.Scalaz._

package object table {

  case class Dot[D, L](value: D, lines: Line[L, D]*)
  case class Line[L, D](value: L, dot: Dot[D, L])

  class Table(val name: String, inColumns: (String, String)*) {
    val columnNames = inColumns.map(_._1)
    require(columnNames.distinct.size == columnNames.size) // カラム名はユニークでなければならない
    val columns: Set[Column] = inColumns.map(c => new Column(c._1, this, c._2)).toSet
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

  class FullColumnInfo(val columnExpression: String, val nowColumnName: String, val originalColumn: Column) {
    def this(column: Column) = this(s"${column.toSql}",
      s"${column.table.name}__${column.name}__${column.typeName}", column)

    val toColumnDefinition = s"$columnExpression AS $nowColumnName"

    def bindUp: FullColumnInfo = new FullColumnInfo(s"GROUP_CONCAT(${this.columnExpression})",
      this.nowColumnName + "s", this.originalColumn)

    def updateTableName(newTableName: String): FullColumnInfo =
      new FullColumnInfo(s"$newTableName.${this.nowColumnName}", this.nowColumnName, this.originalColumn)
  }

  def toSqlFromDot(dot: DotTable): (String, Set[FullColumnInfo]) = {
    val table = dot.value
    val children = dot.lines.zipWithIndex.map { case (line, i) => toSqlFromLine(line, s"_$i") }
    val allFcis = table.columns.map { new FullColumnInfo(_) } ++ children.flatMap(_._2)
    val sql = s"SELECT ${allFcis.getSelectSqlBody} FROM ${table.name}"

    ((sql +: children.map(_._1)).mkString(" "), allFcis)
  }
  private def toSqlFromLine(line: LineJoinDefinition, newChildTableName: String):
  (String, Set[FullColumnInfo]) = {
    val joinDefinition = line.value
    val (sql, oldFcis) = toSqlFromDot(line.dot)
    val fcis = oldFcis.map(_.updateTableName("A"))
    val newFcis = if (joinDefinition._1toNRelation) {
      fcis.map { fci =>
        if (fci.originalColumn == joinDefinition.columnOfChildTable)
          fci
        else
          fci.bindUp
      }
    } else fcis
    val newNewFcis = newFcis.map(_.updateTableName(newChildTableName))
    val shallowNestSql = s"SELECT ${newFcis.getSelectSqlBody} FROM ( $sql ) AS A ${joinDefinition.groupedBy("A")}"
    (s"JOIN ( $shallowNestSql ) AS $newChildTableName ${joinDefinition.toSql(newChildTableName)}", newNewFcis)
  }

  def toTableStructure(jsValue: JsValue): DotTable = {
    def a(jsValue: JsValue): Option[LineJoinDefinition] = jsValue match {
      case JsObject(Some(line), b) =>
        val c = b.values.flatMap(a)
        val d: Seq[LineJoinDefinition] = line.dot.lines
        val e = (c ++ d).toSeq
        LineJoinDefinition(line.value, DotTable(line.dot.value, e: _*)).some
      case JsArray(Some(line), b) =>
        val c = a(b).toSeq
        val d: Seq[LineJoinDefinition] = line.dot.lines
        val e = c ++ d
        LineJoinDefinition(line.value, DotTable(line.dot.value, e: _*)).some
      case _ => None
    }
    a(jsValue).get.dot
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
  // TODO: ここ2次元以上にまだ対応出来ていない(arrayのネストに対応出来ていないという意味)
  def structureSqlResult(sqlResult: List[Map[String, Any]]): List[List[ParsedColumn]] = {
    val a = for (row <- sqlResult) yield {
      for ((columnName, value) <- row) yield {
        val result = """\A(.+)__(.+?)__(.+?)(s*)\Z""".r("tableName", "columnName", "type", "dimention").
          findFirstMatchIn(columnName).get

        val dim = result.group("dimention").count(_ == 's')
        val parsedValue = if (dim == 1) {
          // Any or Array[Any] or Array[Array[Any]] or ...
          val splitted = value.asInstanceOf[String].split(',')
          result.group("type") match {
            case "Int" => splitted.map(_.toInt) // 下と違って一度group_concatしているためstringからtoIntする必要がある
            case "String" => splitted // .asInstanceOf[String]
          }
        } else {
          result.group("type") match {
            case "Int" => value.asInstanceOf[Int]
            case "String" => value.asInstanceOf[String]
          }
        }

        ParsedColumn(result.group("tableName"), result.group("columnName"), dim, parsedValue)
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
          require(arrayLengths.length == 1, row)
          (0 until arrayLengths(0)).map { index =>
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
