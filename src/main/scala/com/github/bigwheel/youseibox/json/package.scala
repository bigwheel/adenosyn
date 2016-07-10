package com.github.bigwheel.youseibox

// あとでstructureとかそのままのjsonじゃなくてjsonの構造を定義するものだという意味を名前にきちんと込める
// TableとかJoinDefinitionも追加するならDSLとか包括的な意味にしたらどうだろう
package object json {

  sealed trait JsValue

  final case class JsObject(
    tableOption: Option[JoinDefinitionBase], // TODO: あとでここリネーム
    properties: Map[String, JsValue]) extends JsValue
  final case class JsString(tableName: String, columnName: String) extends JsValue
  final case class JsInt(tableName: String, columnName: String) extends JsValue
  final case class JsArray(tableOption: Option[JoinDefinitionBase], elem: JsValue) extends JsValue

  type ColumnName = String
  type ScalaTypeName = String
  case class Table(name: String, joinDefinitions: JoinDefinitionBase*) {
    def digUpTables: Seq[Table] = this +: joinDefinitions.flatMap(_.digUpTables)
  }
  class TableWithColumns(table: Table, columns: Seq[(ColumnName, ScalaTypeName)]) extends
    Table(table.name, table.joinDefinitions: _*)
  sealed trait JoinDefinitionBase {
    val childSide: Table
    def copy(childSide: Table): JoinDefinitionBase
    def digUpTables: Seq[Table] = childSide.digUpTables
  }
  case class RootJoinDefinition(childSide: Table) extends JoinDefinitionBase {
    override def copy(childSide: Table): JoinDefinitionBase = new RootJoinDefinition(childSide)
  }
  case class JoinDefinition(
    parentSideColumn: (ColumnName, ScalaTypeName),
    groupBy: Boolean,
    childSideColumn: (ColumnName, ScalaTypeName),
    childSide: Table
  ) extends JoinDefinitionBase {
    override def copy(childSide: Table): JoinDefinitionBase =
      new JoinDefinition(this.parentSideColumn, this.groupBy, this.childSideColumn, childSide)
  }

}
