package com.github.bigwheel.youseibox

import scalaz._
import Scalaz._

package object table {

  case class ChainedTable(name: String, columnOfParentTable: String, columnOfChainedTable: String)
  case class _1toNTable(name: String, columnOfParentTable: String, columnOfChainedTable: String)

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
      val postfix = if (_1toNRelation) s" GROUP BY ${columnOfParentTable.toSql}" else ""
      s"ON ${columnOfParentTable.toSql} = $newChildTableName.$newChildColumnName$postfix"
    }
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

  private def tableToSqlPlusColumnInfo(table: Table): (String, Set[FullColumnInfo]) = {
    val fullColumnInfoSet = for (column <- table.columns) yield new FullColumnInfo(column)

    (s"SELECT ${fullColumnInfoSet.getSelectSqlBody} FROM ${table.name}", fullColumnInfoSet)
  }

  def toSqlFromDot(dot: Dot[Table, JoinDefinition]): (String, Set[FullColumnInfo]) = {
    val table = dot.value
    val children = dot.lines.map { toSqlFromLine(_, "A") }
    val allFcis = table.columns.map { new FullColumnInfo(_) } ++ children.flatMap(_._2)
    val sql = s"SELECT ${allFcis.getSelectSqlBody} FROM ${table.name}"

    ((sql +: children.map(_._1)).mkString(" "), allFcis)
  }
  def toSqlFromLine(line: Line[JoinDefinition, Table], newChildTableName: String):
  (String, Set[FullColumnInfo]) = {
    val joinDefinition = line.value
    val (sql, fcis) = toSqlFromDot(line.dot)
    val newFcis = fcis.map(_.updateTableName(newChildTableName))
    // TODO: ここ、join対象のカラムはgroup_concatしないように改良するべきか？
    // join元があるしそこまでする必要がない気もする
    val newNewFcis = if (joinDefinition._1toNRelation) newFcis.map(_.bindUp) else newFcis
    (s"JOIN ( $sql ) AS $newChildTableName ${joinDefinition.toSql(newChildTableName)}", newNewFcis)
  }

  // 返り値2つ目はカラム名とそのオリジナルのテーブル名・カラム名
  def toSql(tableTree: Dot[Table, JoinDefinition]): (String, Set[FullColumnInfo]) = {
    if (tableTree.lines.isEmpty) {
      tableToSqlPlusColumnInfo(tableTree.value)
    } else {
      val parentTable = tableTree.value
      val joinDefinition = tableTree.lines.head.value
      val childTable = tableTree.lines.head.dot.value

      val parentSide = joinDefinition.columnOfParentTable.toSql

      if (tableTree.lines.head.dot.lines.isEmpty) {
        val FCIsForParentSelect = {
          val parentTableFCIs = parentTable.columns.map(new FullColumnInfo(_))
          val childTableFCIsForParentSelect = childTable.columns.map(new FullColumnInfo(_)).map { fci =>
            if (joinDefinition._1toNRelation)
              fci.bindUp
            else
              fci
          }
          parentTableFCIs ++ childTableFCIsForParentSelect
        }

        val childSide = childTable.name + "." + joinDefinition.columnOfChildTable.name

        val postfix = if (joinDefinition._1toNRelation) s" GROUP BY $parentSide" else ""
        (s"SELECT ${FCIsForParentSelect.getSelectSqlBody} FROM ${parentTable.name} JOIN ${childTable.name} " +
          s"ON $parentSide = $childSide$postfix", FCIsForParentSelect)
      } else {
        val temporaryTableName = "A"

        // 子テーブル系の処理
        val (sql, childTableFCIs) = toSql(tableTree.lines.head.dot)
        val nestedTableSql = "( " + sql + " ) AS " + temporaryTableName

        // 親テーブルのためのFCIsの算出
        val FCIsForParentSelect = {
          val parentTableFCIs = parentTable.columns.map(new FullColumnInfo(_))
          val childTableFCIsForParentSelect = childTableFCIs.map(_.updateTableName(temporaryTableName)).map { fci =>
            if (joinDefinition._1toNRelation)
              fci.bindUp
            else
              fci
          }
          parentTableFCIs ++ childTableFCIsForParentSelect
        }

        val childSide = temporaryTableName + "." + childTable.name + "__" +
          joinDefinition.columnOfChildTable.name

        val postfix = if (joinDefinition._1toNRelation) s" GROUP BY $parentSide" else ""
        (s"SELECT ${FCIsForParentSelect.getSelectSqlBody} FROM ${parentTable.name} JOIN $nestedTableSql " +
          s"ON $parentSide = $childSide$postfix", FCIsForParentSelect)
      }
    }
  }
}
