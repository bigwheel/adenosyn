package com.github.bigwheel.youseibox

package object table {

  trait Table {
    val name: String
    val chainTable: Option[_1to1Table]
    def chainTables: Seq[_1to1Table] = chainTable.map{ct => ct +: ct.chainTables}.getOrElse(Nil)
  }

  case class RootTable(
    val name: String,
    val chainTable: Option[_1to1Table] = None
  ) extends Table

  object _1to1Table {
    def apply(name: String, joinRule: String, chainTable: Option[_1to1Table] = None): _1to1Table =
      new _1to1Table(name, joinRule, chainTable)
  }

  class _1to1Table(
    val name: String,
    joinRule: String,
    val chainTable: Option[_1to1Table] = None
  ) extends Table {
    val joinString = s"JOIN $name ON $joinRule"
  }

  case class _1toNTable(
    val name: String,
    val joinColumnName: String,
    val parentColumnName: String,
    val chainTable: Option[_1to1Table] = None
  ) extends Table

}
