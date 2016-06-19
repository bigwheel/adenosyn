package com.github.bigwheel.youseibox

package object table {

  case class OldTable(name: String, chainedTableOption: Option[ChainedTable] = None) {
    def definition = chainedTableOption match {
      case Some(ct) => s"$name JOIN ${ct.name} ON" +
        s" $name.${ct.columnOfParentTable} = ${ct.name}.${ct.columnOfChainedTable}"
      case None => name
    }
  }

  case class ChainedTable(name: String, columnOfParentTable: String, columnOfChainedTable: String)
  case class _1toNTable(name: String, columnOfParentTable: String, columnOfChainedTable: String)

  case class Dot[D, L](value: D, lines: Line[L, D]*)
  case class Line[L, D](value: L, child: Dot[D, L])

  class Table(val name: String, columnNames: String*) {
    require(columnNames.distinct.size == columnNames.size) // カラム名はユニークでなければならない
    val columns: Set[Column] = columnNames.map(new Column(_, this)).toSet
    def getColumn(name: String): Option[Column] = columns.find(_ == name)
  }

  class Column(val name: String, val table: Table)
  case class JoinDefinition(
    columnNameOfParentTable: String,
    _1toNRelation: Boolean,
    columnNameOfChildTable: String)

  case class FullColumnInfo(columnExpression: String, nowColumnName: String, originalColumn: Column) {
    val toColumnDefinition = s"$columnExpression AS $nowColumnName"
  }
  // 返り値2つ目はカラム名とそのオリジナルのテーブル名・カラム名
  def toSql(tableStructure: Dot[Table, JoinDefinition]): (String, Set[FullColumnInfo])  = {
    if (tableStructure.lines.isEmpty) {
      val parentTable = tableStructure.value
      val parentTableColumns = parentTable.columns.map { column =>
        FullColumnInfo(s"${parentTable.name}.${column.name}", s"${parentTable.name}__${column.name}",
          column)
      }
      val columnsDefinition = parentTableColumns.map(_.toColumnDefinition).mkString(", ")

      (s"SELECT $columnsDefinition FROM ${parentTable.name}", parentTableColumns)
    } else if (tableStructure.lines.head.child.lines.nonEmpty) {
      val (sql, fullColumnsInfos) = toSql(tableStructure.lines.head.child)

      val temporaryTableName = "A"

      val dot = tableStructure

      val parentTable = dot.value
      val parentTableColumns = parentTable.columns.map { column =>
        FullColumnInfo(s"${parentTable.name}.${column.name}", s"${parentTable.name}__${column.name}",
          column)
      }
      val line = dot.lines.head
      val childTableColumns = fullColumnsInfos.map { fci =>
        val base = s"$temporaryTableName.${fci.nowColumnName}"
        if (line.value._1toNRelation)
          FullColumnInfo(s"GROUP_CONCAT($base)", s"${fci.nowColumnName}s", fci.originalColumn)
        else
          FullColumnInfo(base, fci.nowColumnName, fci.originalColumn)
      }

      val columnsDefinition = (parentTableColumns ++ childTableColumns).map(_.toColumnDefinition).mkString(", ")
      val nestedTable = "( " + sql + " ) AS " + temporaryTableName

      val parentSide = parentTable.name + "." + line.value.columnNameOfParentTable
      val childSide = temporaryTableName + "." + tableStructure.lines.head.child.value.name + "__" +
        line.value.columnNameOfChildTable

      val postfix = if (line.value._1toNRelation) s" GROUP BY $parentSide" else ""
      (s"SELECT $columnsDefinition FROM ${parentTable.name} JOIN $nestedTable " +
        s"ON $parentSide = $childSide$postfix", parentTableColumns ++ childTableColumns)
    } else {
      val dot = tableStructure

      val parentTable = dot.value
      val parentTableColumns = parentTable.columns.map { column =>
        FullColumnInfo(s"${parentTable.name}.${column.name}", s"${parentTable.name}__${column.name}",
          column)
      }
      val line = dot.lines.head
      val childTable = line.child.value
      val childTableColumns = childTable.columns.map { column =>
        val base = s"${childTable.name}.${column.name}"
        if (line.value._1toNRelation)
          FullColumnInfo(s"GROUP_CONCAT($base)", s"${childTable.name}__${column.name}s", column)
        else
          FullColumnInfo(base, s"${childTable.name}__${column.name}", column)
      }

      val columnsDefinition = (parentTableColumns ++ childTableColumns).map(_.toColumnDefinition).mkString(", ")

      val parentSide = parentTable.name + "." + line.value.columnNameOfParentTable
      val childSide = childTable.name + "." + line.value.columnNameOfChildTable

      val postfix = if (line.value._1toNRelation) s" GROUP BY $parentSide" else ""
      (s"SELECT $columnsDefinition FROM ${parentTable.name} JOIN ${childTable.name} " +
        s"ON $parentSide = $childSide$postfix", parentTableColumns ++ childTableColumns)
    }
  }
}
