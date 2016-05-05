package com.github.bigwheel.youseibox

package object table {

  case class Table(name: String, chainedTableOption: Option[ChainedTable] = None) {
    def definition = chainedTableOption match {
      case Some(ct) => s"$name JOIN ${ct.name} ON" +
        s" $name.${ct.columnOfParentTable} = ${ct.name}.${ct.columnOfChainedTable}"
      case None => name
    }
  }

  case class ChainedTable(name: String, columnOfParentTable: String, columnOfChainedTable: String)

  case class _1toNTable(name: String, columnOfParentTable: String, columnOfChainedTable: String)
}
