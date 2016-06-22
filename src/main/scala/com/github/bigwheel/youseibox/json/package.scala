package com.github.bigwheel.youseibox

import com.github.bigwheel.youseibox.table.DotTable

// あとでstructureとかそのままのjsonじゃなくてjsonの構造を定義するものだという意味を名前にきちんと込める
package object json {

  trait JsValue

  case class JsObject(
    tableOption: Option[DotTable],
    properties: Map[String, JsValue]
  ) extends JsValue

  case class JsString(tableName: String, columnName: String) extends JsValue
  case class JsInt(tableName: String, columnName: String) extends JsValue
  case class JsArray(elem: JsValue) extends JsValue
}
