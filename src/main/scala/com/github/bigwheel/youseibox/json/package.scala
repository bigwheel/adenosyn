package com.github.bigwheel.youseibox

import com.github.bigwheel.youseibox.table.Dot
import com.github.bigwheel.youseibox.table.JoinDefinition
import com.github.bigwheel.youseibox.table.Table

// あとでstructureとかそのままのjsonじゃなくてjsonの構造を定義するものだという意味を名前にきちんと込める
package object json {

  trait JsValue

  case class JsObject(
    tableOption: Option[Dot[Table, JoinDefinition]],
    properties: Map[String, JsValue]
  ) extends JsValue

  case class JsString(tableName: String, columnName: String) extends JsValue
  case class JsInt(tableName: String, columnName: String) extends JsValue
  case class JsArray(elem: JsValue) extends JsValue
}
