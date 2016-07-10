package com.github.bigwheel.youseibox

import com.github.bigwheel.youseibox.table.JoinDefinitionForConstruct

// あとでstructureとかそのままのjsonじゃなくてjsonの構造を定義するものだという意味を名前にきちんと込める
// TableとかJoinDefinitionも追加するならDSLとか包括的な意味にしたらどうだろう
package object json {

  sealed trait JsValue

  case class JsObject(
    tableOption: Option[JoinDefinitionForConstruct],
    properties: Map[String, JsValue]) extends JsValue
  case class JsString(tableName: String, columnName: String) extends JsValue
  case class JsInt(tableName: String, columnName: String) extends JsValue
  case class JsArray(tableOption: Option[JoinDefinitionForConstruct], elem: JsValue) extends JsValue

  type ColumnName = String
  type ScalaTypeName = String
  case class Table(name: String, joinDefinitions: JoinDefinition*)
  case class JoinDefinition(
    parentSideColumn: (ColumnName, ScalaTypeName),
    groupBy: Boolean,
    childSideColumn: (ColumnName, ScalaTypeName),
    childSide: Table
  )
}
