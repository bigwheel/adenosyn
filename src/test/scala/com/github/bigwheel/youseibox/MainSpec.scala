package com.github.bigwheel.youseibox

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import org.scalatest._
import scala.io.Source
import scala.sys.process._

class MainSpec extends FunSpec with Matchers {
  val ipAddress = Source.fromFile(".otto/data/dev_ip").getLines.next

  it("開発環境VMが動いている") {
    val result = Process("otto dev vagrant status").!!
    result.split("\n")(3) should
      equal("default                   running (virtualbox)")
  }

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
    Main.db(ipAddress)
  }
}
