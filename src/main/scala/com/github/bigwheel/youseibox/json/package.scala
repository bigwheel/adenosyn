package com.github.bigwheel.youseibox

import com.github.bigwheel.youseibox.table.JoinDefinition

// あとでstructureとかそのままのjsonじゃなくてjsonの構造を定義するものだという意味を名前にきちんと込める
package object json {

  trait JsValue

  case class JsObject(
    tableOption: Option[JoinDefinition],
    properties: Map[String, JsValue]) extends JsValue
  case class JsString(tableName: String, columnName: String) extends JsValue
  case class JsInt(tableName: String, columnName: String) extends JsValue
  case class JsArray(tableOption: Option[JoinDefinition], elem: JsValue) extends JsValue
}
