package com.github.bigwheel.youseibox

import com.github.bigwheel.youseibox.table.LineJoinDefinition

// あとでstructureとかそのままのjsonじゃなくてjsonの構造を定義するものだという意味を名前にきちんと込める
package object json {

  trait JsValue

  case class JsObject(
    tableOption: Option[LineJoinDefinition],
    properties: Map[String, JsValue]) extends JsValue
  case class JsString(tableName: String, columnName: String) extends JsValue
  case class JsInt(tableName: String, columnName: String) extends JsValue
  case class JsArray(tableOption: Option[LineJoinDefinition], elem: JsValue) extends JsValue
}
