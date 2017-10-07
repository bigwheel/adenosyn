package com.github.bigwheel.adenosyn

import scala.io.Source
import scala.language.reflectiveCalls

// http://www.ne.jp/asahi/hishidama/home/tech/scala/sample/using.html
object Using {
  def using[A, B](resource: A)(func: A => B)(implicit closer: Closer[A]) =
    try {
      func(resource)
    } finally {
      closer(resource)
    }

  trait Closer[-A] {
    def apply(resource: A)
  }

  object Closer {
    def apply[A](f: A => Unit) = new Closer[A] {
      def apply(resource: A) = f(resource)
    }
  }

  implicit val IoSourceCloser = Closer[Source]         (_.close)
  implicit val disposer       = Closer[{def dispose()}](_.dispose)
  implicit val closer         = Closer[{def close()}]  (_.close)
}
