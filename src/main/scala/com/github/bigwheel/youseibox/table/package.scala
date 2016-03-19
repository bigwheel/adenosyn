package com.github.bigwheel.youseibox

package object table {

  trait Table {
    val name: String
    val chainTable: Option[_1to1Table]
  }

  case class RootTable(
    val name: String,
    val chainTable: Option[_1to1Table] = None
  ) extends Table

  case class _1to1Table(
    val name: String,
    val joinRule: String,
    val chainTable: Option[_1to1Table] = None
  ) extends Table

  case class _1toNTable(
    val name: String,
    val joinColumnName: String,
    val parentColumnName: String,
    val chainTable: Option[_1to1Table] = None
  ) extends Table

}
