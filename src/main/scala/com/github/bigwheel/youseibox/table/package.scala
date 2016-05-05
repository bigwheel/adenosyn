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

  def toSql(tableStructure: Dot[Table, JoinDefinition]): String = {
    val parentTable = tableStructure.value
    val parentTableColumns = parentTable.columnNames.map(parentTable.name + "." + _)
    val line = tableStructure.lines.head
    val childTable = line.child.value
    val childTableColumns = {
      val base = childTable.columnNames./* filter(_ != line.value.columnNameOfChildTable).*/
        map(childTable.name + "." + _)
      if (line.value._1toNRelation)
        base.map("GROUP_CONCAT(" + _ + ")")
      else
        base
    }
    val columnNames = parentTableColumns ++ childTableColumns

    val parentSide = parentTable.name + "." + line.value.columnNameOfParentTable
    val childSide = childTable.name + "." + line.value.columnNameOfChildTable

    val postfix = if (line.value._1toNRelation) s" GROUP BY $parentSide" else ""
    "SELECT " + columnNames.mkString(", ") + s" FROM ${parentTable.name} JOIN ${childTable.name} " +
    s"ON $parentSide = $childSide$postfix"
  }
}
