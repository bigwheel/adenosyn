package com.github.bigwheel.youseibox

package object table {

  trait Table {
    val name: String
    protected[this] val chainTable: Option[_1to1Table]
    def getJoinStrings: String = chainTable.map(_.joinStrings).getOrElse("")
  }

  object RootTable {
    def apply(name: String, chainTable: Option[_1to1Table] = None) = new RootTable(name, chainTable)
  }

  class RootTable(
    val name: String,
    protected[this] val chainTable: Option[_1to1Table] = None
  ) extends Table

  object _1to1Table {
    def apply(name: String, joinRule: String, chainTable: Option[_1to1Table] = None): _1to1Table =
      new _1to1Table(name, joinRule, chainTable)
  }

  class _1to1Table(
    val name: String,
    joinRule: String,
    protected[this] val chainTable: Option[_1to1Table] = None
  ) extends Table {
    val joinStrings: String = s"JOIN $name ON $joinRule\n" + chainTable.map(_.joinStrings).getOrElse("")
  }

  object _1toNTable {
    def apply(name: String, joinColumnName: String, parentColumnName: String,
      chainTable: Option[_1to1Table] = None) =
      new _1toNTable(name, joinColumnName, parentColumnName, chainTable)
  }

  class _1toNTable(
    val name: String,
    val joinColumnName: String,
    val parentColumnName: String,
    protected[this] val chainTable: Option[_1to1Table] = None
  ) extends Table

}
