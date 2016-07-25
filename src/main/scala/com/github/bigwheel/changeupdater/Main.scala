package com.github.bigwheel.changeupdater

import java.util.concurrent.ScheduledFuture
import java.util.concurrent.ScheduledThreadPoolExecutor
import java.util.concurrent.TimeUnit

object Main {

  def main(args: Array[String]): Unit = {
    val e = new ScheduledThreadPoolExecutor(1)
    val a: ScheduledFuture[_] = e.scheduleAtFixedRate(task, 1, 60, TimeUnit.SECONDS)
  }

  val task = new Runnable {
    def run() = println("beep!")
  }

}
