package com.github.bigwheel.adenosyn

import com.github.bigwheel.adenosyn.changerecorder.JdbcUrl
import scala.io.Source
import scalikejdbc._

package object sqlutil {

  def url(dbName: String = "") = s"jdbc:mysql://127.0.0.1/$dbName?useSSL=false"

  def jdbcUrlForTest(dbName: String) = new JdbcUrl(sqlutil.url(dbName), dbName)

  implicit class RichString(q: String)(implicit session: DBSession) {
    def query(): Unit = SQL(q).execute.apply()
  }

  implicit class RichSeqString(queries: Seq[String])(implicit session: DBSession) {
    def query(): Unit = queries.foreach(_.query())
  }

  def suppressLog() = GlobalSettings.loggingSQLAndTime = LoggingSQLAndTimeSettings(
    enabled = false,
    warningEnabled = true,
    warningThresholdMillis = 1000L,
    warningLogLevel = 'WARN
  )

  def executeStatements(statements: String)(implicit session: DBSession): Unit =
    statements.split(';').toSeq.query()

  def executeScript(resourcePath: String)(implicit session: DBSession): Unit =
    executeStatements(Source.fromURL(getClass.getResource(resourcePath)).getLines.mkString("\n"))

  def executeInstantly(url: String, user: String, password: String, statements: String): Unit =
    using(Commons2ConnectionPoolFactory(url, user, password)) { rootPool =>
      using(DB(rootPool.borrow)) { ghostDb =>
        ghostDb.autoCommit { implicit session => executeStatements(statements) }
      }
    }

}
