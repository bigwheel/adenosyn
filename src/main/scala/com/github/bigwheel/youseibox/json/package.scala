package com.github.bigwheel.youseibox

package object json {

  trait JsValue

  abstract class JsString extends JsValue

  abstract class JsInt extends JsValue

  abstract class JsArray extends JsValue

  abstract class JsObject extends JsValue

}
