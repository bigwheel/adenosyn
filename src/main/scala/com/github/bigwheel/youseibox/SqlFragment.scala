package com.github.bigwheel.youseibox

object SqlFragment {
  def apply(selectMain: String, joinFragment: String): SqlFragment =
    this.apply(Nil, selectMain, Some(joinFragment), Nil)

  def apply(selectMain: String): SqlFragment =
    this.apply(Nil, selectMain, None, Nil)
}

case class SqlFragment(
  preProcess: Seq[String],
  selectMain: String,
  joinFragment: Option[String],
  postProcess: Seq[String]
)
