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

  case class Table(name: String, columnName: String*)
  case class JoinDefinition(
    columnNameOfParentTable: String,
    //_1toNRelation: Boolean,
    columnNameOfChildTable: String)

  def toSql(tableStructure: Dot[Table, JoinDefinition]): String = {
    def lineToSql(parentTable: Table, line: Line[JoinDefinition, Table]): String = {
      s"JOIN ${line.child.value.name} " +
        s"ON ${parentTable.name}.${line.value.columnNameOfParentTable} " +
        s"= ${line.child.value.name}.${line.value.columnNameOfChildTable}"
    }
    def dotToSql(dot: Dot[Table, JoinDefinition]): String =
      (dot.value.name +: dot.lines.map(lineToSql(dot.value, _))).map(_.trim).mkString(" ")

    "SELECT * FROM " + dotToSql(tableStructure)
  }
}
