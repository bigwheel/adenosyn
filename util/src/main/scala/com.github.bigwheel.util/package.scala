package com.github.bigwheel

import scala.io.Source
import scalikejdbc.DB
import scalikejdbc.DBSession
import scalikejdbc.GlobalSettings
import scalikejdbc.LoggingSQLAndTimeSettings
import scalikejdbc.SQL

package object util {

  implicit class RichString(q: String) {
    def query() = DB.autoCommit { implicit session => SQL(q).execute.apply() }
  }

  implicit class RichSeqString(queries: Seq[String]) {
    def query() = DB.autoCommit { implicit session => queries.foreach(SQL(_).execute.apply()) }
  }

  def suppressSqlLog() = {
    GlobalSettings.loggingSQLAndTime = LoggingSQLAndTimeSettings(
      enabled = false,
      warningEnabled = true,
      warningThresholdMillis = 1000L,
      warningLogLevel = 'WARN
    )
  }

  def executeSqlScript(resourcePath: String)(implicit session: DBSession): Unit = {
    val sqlStatements: Seq[String] = Source.fromURL(getClass.getResource(resourcePath)).getLines.
      mkString("\n").split(';')
    sqlStatements.query()
  }
}
