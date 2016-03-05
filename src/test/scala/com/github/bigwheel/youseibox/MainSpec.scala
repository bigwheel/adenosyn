package com.github.bigwheel.youseibox

import org.scalatest._

class MainSpec extends FunSpec with Matchers {
  it("Main.mainを呼べる") {
    Main.main(Array[String]())
  }
}
