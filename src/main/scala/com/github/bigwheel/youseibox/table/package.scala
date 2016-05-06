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

  case class Table(name: String, columnNames: String*)
  case class JoinDefinition(
    columnNameOfParentTable: String,
    _1toNRelation: Boolean,
    columnNameOfChildTable: String)

  def toSql(tableStructure: Dot[Table, JoinDefinition]): (String, Seq[String])  = {
    if (tableStructure.lines.head.child.lines.nonEmpty) {
      val (sql, ccns) = toSql(tableStructure.lines.head.child)

      val temporaryTableName = "A"

      val dot = tableStructure

      val parentTable = dot.value
      val parentTableColumns = parentTable.columnNames.map { columnName =>
        (s"${parentTable.name}.$columnName", s"${parentTable.name}__$columnName")
      }
      val line = dot.lines.head
      val childTableColumns = ccns.map { columnName =>
        val base = s"$temporaryTableName.$columnName"
        if (line.value._1toNRelation)
          (s"GROUP_CONCAT($base)", s"${columnName}s")
        else
          (base, columnName)
      }

      val columnNames = (parentTableColumns ++ childTableColumns).map(c => c._1 + " AS " + c._2)
      val nestedTable = "( " + sql + " ) AS " + temporaryTableName

      val parentSide = parentTable.name + "." + line.value.columnNameOfParentTable
      val childSide = temporaryTableName + "." + tableStructure.lines.head.child.value.name + "__" +
        line.value.columnNameOfChildTable

      val postfix = if (line.value._1toNRelation) s" GROUP BY $parentSide" else ""
      ("SELECT " + columnNames.mkString(", ") + s" FROM ${parentTable.name} JOIN $nestedTable " +
        s"ON $parentSide = $childSide$postfix", (parentTableColumns ++ childTableColumns).map(_._2))
    } else {
      val dot = tableStructure

      val parentTable = dot.value
      val parentTableColumns = parentTable.columnNames.map { columnName =>
        (s"${parentTable.name}.$columnName", s"${parentTable.name}__$columnName")
      }
      val line = dot.lines.head
      val childTable = line.child.value
      val childTableColumns = childTable.columnNames.map { columnName =>
        val base = s"${childTable.name}.$columnName"
        if (line.value._1toNRelation)
          (s"GROUP_CONCAT($base)", s"${childTable.name}__${columnName}s")
        else
          (base, s"${childTable.name}__$columnName")
      }

      val columnNames = (parentTableColumns ++ childTableColumns).map(c => c._1 + " AS " + c._2)

      val parentSide = parentTable.name + "." + line.value.columnNameOfParentTable
      val childSide = childTable.name + "." + line.value.columnNameOfChildTable

      val postfix = if (line.value._1toNRelation) s" GROUP BY $parentSide" else ""
      ("SELECT " + columnNames.mkString(", ") + s" FROM ${parentTable.name} JOIN ${childTable.name} " +
        s"ON $parentSide = $childSide$postfix", (parentTableColumns ++ childTableColumns).map(_._2))
    }
  }
}
