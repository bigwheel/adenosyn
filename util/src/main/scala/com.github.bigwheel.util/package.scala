package com.github.bigwheel

import scala.io.Source
import scalikejdbc.Commons2ConnectionPoolFactory
import scalikejdbc.DB
import scalikejdbc.DBSession
import scalikejdbc.GlobalSettings
import scalikejdbc.LoggingSQLAndTimeSettings
import scalikejdbc.SQL
import scalikejdbc.using

package object util {

  implicit class RichString(q: String)(implicit session: DBSession) {
    def query(): Unit = SQL(q).execute.apply()
  }

  implicit class RichSeqString(queries: Seq[String])(implicit session: DBSession) {
    def query(): Unit = queries.foreach(_.query())
  }

  def suppressSqlLog() = {
    GlobalSettings.loggingSQLAndTime = LoggingSQLAndTimeSettings(
      enabled = false,
      warningEnabled = true,
      warningThresholdMillis = 1000L,
      warningLogLevel = 'WARN
    )
  }

  def executeSqlStatements(statements: String)(implicit session: DBSession): Unit =
    statements.split(';').toSeq.query()

  def executeSqlScript(resourcePath: String)(implicit session: DBSession): Unit =
    executeSqlStatements(Source.fromURL(getClass.getResource(resourcePath)).getLines.mkString("\n"))

  def executeSqlInstantly(url: String, user: String, password: String, statements: String): Unit =
    using(Commons2ConnectionPoolFactory(url, user, password)) { rootPool =>
      using(DB(rootPool.borrow)) { ghostDb =>
        ghostDb.autoCommit { implicit session => util.executeSqlStatements(statements) }
      }
    }

}
