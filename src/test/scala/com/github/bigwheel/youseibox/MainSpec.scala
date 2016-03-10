package com.github.bigwheel.youseibox

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import org.scalatest._
import scala.sys.process._

class MainSpec extends FunSpec with Matchers {
  it("Main.mainを呼べる") {
    val outStream = new ByteArrayOutputStream
    val out = new PrintStream(outStream, true, "utf-8")
    Console.withOut(out) {
      Main.main(Array[String]())
      out.flush()
      outStream.toString("utf-8") shouldEqual "hello\n"
    }
  }

  it("abc") {
  }
}
