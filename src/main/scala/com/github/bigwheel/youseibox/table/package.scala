package com.github.bigwheel.youseibox

import argonaut.Argonaut._
import argonaut._
import com.github.bigwheel.youseibox.json._
import scalaz.Scalaz._
import scalaz._

package object table {

  case class Dot[D, L](value: D, lines: Line[L, D]*)
  case class Line[L, D](value: L, dot: Dot[D, L])

  class Table(val name: String, columnNames: String*) {
    require(columnNames.distinct.size == columnNames.size) // カラム名はユニークでなければならない
    val columns: Set[Column] = columnNames.map(new Column(_, this)).toSet
    def getColumn(name: String): Column = columns.find(_.name === name).get
  }

  class Column(val name: String, val table: Table) {
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
    def this(column: Column) = this(s"${column.toSql}", s"${column.table.name}__${column.name}", column)

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

  def toJsonObj(sqlResult: List[Map[String, Any]], jsonStructure: JsValue): List[Json] = {
    def f(row: Map[String, Any], jsonTree: JsValue): Json = jsonTree match {
      case JsObject(_, properties) =>
        Json(properties.map { case (k, v) => k := f(row, v) }.toSeq: _*)
      case JsString(tableName, columnName) =>
        jString(row(tableName + "__" + columnName).asInstanceOf[String])
      case JsInt(tableName, columnName) =>
        jNumber(row(tableName + "__" + columnName).asInstanceOf[Int])
      case JsArray(_, JsString(tableName, columnName)) =>
        val a = row(tableName + "__" + columnName + "s").asInstanceOf[String].split(",")
        Json.array(a.map(jString.apply): _*)
      case JsArray(_, JsInt(tableName, columnName)) =>
        val a = row(tableName + "__" + columnName + "s").asInstanceOf[String].split(",")
        Json.array(a.map(_.asInstanceOf[Int]).map(jNumber): _*)
    }

    for (row <- sqlResult) yield f(row, jsonStructure)
  }

}
