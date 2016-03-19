package com.github.bigwheel.youseibox

package object table {

  trait Table {
    val name: String
    val chainTable: Option[LeafOneToOneTable]
  }

  case class RootTable(
    val name: String,
    val chainTable: Option[LeafOneToOneTable] = None
  ) extends Table

  case class LeafOneToOneTable(
    val name: String,
    val joinRule: String,
    val chainTable: Option[LeafOneToOneTable] = None
  ) extends Table

  case class LeafOneToManyTable(
    val name: String,
    val childColumnForJoin: String,
    val parentColumnForGroupBy: String,
    val chainTable: Option[LeafOneToOneTable] = None
  ) extends Table

}
