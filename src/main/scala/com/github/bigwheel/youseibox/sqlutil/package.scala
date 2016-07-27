package com.github.bigwheel.youseibox

import com.sksamuel.elastic4s.ElasticsearchClientUri
import scala.io.Source
import scala.sys.process.Process
import scalikejdbc.Commons2ConnectionPoolFactory
import scalikejdbc.DB
import scalikejdbc.DBSession
import scalikejdbc.GlobalSettings
import scalikejdbc.LoggingSQLAndTimeSettings
import scalikejdbc.SQL
import scalikejdbc.using

package object sqlutil {

  private lazy val ipAddress = Process("otto dev address").!!.stripLineEnd

  /**
    * どう考えてもここにあるべきじゃない・・
    */
  def elasticsearchUrl: ElasticsearchClientUri = ElasticsearchClientUri(ipAddress, 9200)

  def url(dbName: String = "") = s"jdbc:mysql://$ipAddress/$dbName?useSSL=false"

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
