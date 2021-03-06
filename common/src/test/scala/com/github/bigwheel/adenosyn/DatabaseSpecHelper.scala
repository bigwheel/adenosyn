package com.github.bigwheel.adenosyn

import com.github.bigwheel.adenosyn.sqlutil._
import org.scalatest.Suite
import scalikejdbc._

trait DatabaseSpecHelper { this: Suite =>

  Class.forName("com.mysql.jdbc.Driver")

  // http://qiita.com/suin/items/5a7a56afacc8a35abcb6
  private[this] def md5(text: String): String = java.security.MessageDigest.
    getInstance("MD5").digest(text.getBytes).map("%02x".format(_)).mkString

  protected[this] val postfix = md5(this.getClass().getCanonicalName()).take(8)
  protected[this] val observeeDbName = "observee" + postfix
  protected[this] val changeLogDbName = "changelog" + postfix
  protected[this] val userName = "changeloggermanager" + postfix
  protected[this] val password = "clm" + postfix

  protected[this] val defaultDbConnectionPool =
    Commons2ConnectionPoolFactory(sqlutil.url(), "root", "root")
  protected[this] val observeeDbConnectionPool =
    Commons2ConnectionPoolFactory(sqlutil.url(observeeDbName), "root", "root")
  protected[this] val changeLogDbConnectionPool =
    Commons2ConnectionPoolFactory(sqlutil.url(changeLogDbName), "root", "root")

  override def finalize(): Unit = {
    defaultDbConnectionPool.close
    observeeDbConnectionPool.close
    changeLogDbConnectionPool.close
  }

  protected[this] def usingDb
    (connectionPool: Commons2ConnectionPool = defaultDbConnectionPool)
    (method: DB => Any): Unit =
    using(DB(connectionPool.borrow())) { db => method(db) }

  protected[this] def readOnly
    (connectionPool: Commons2ConnectionPool = defaultDbConnectionPool)
    (test: DBSession => Any): Unit =
    usingDb(connectionPool)(_.readOnly { session => test(session) })

  protected[this] def autoCommit
    (connectionPool: Commons2ConnectionPool = defaultDbConnectionPool)
    (test: DBSession => Any): Unit =
    usingDb(connectionPool)(_.autoCommit { session => test(session) })

  protected[this] def withNoDatabases(test: => Any) {
    autoCommit() { implicit session =>
      sqlutil.executeStatements(
        s"""DROP DATABASE IF EXISTS $observeeDbName;
           |DROP DATABASE IF EXISTS $changeLogDbName;
           |DROP USER IF EXISTS '$userName'@'%';""".stripMargin
      )
    }
    test
  }

  protected[this] def withDatabases(test: => Any) {
    withNoDatabases {
      autoCommit() { implicit session =>
        sqlutil.executeStatements(
          s"""CREATE DATABASE $observeeDbName;
             |CREATE DATABASE $changeLogDbName;""".stripMargin
        )
      }
      test
    }
  }

  protected[this] def withUserAndDatabases(test: => Any) {
    withDatabases {
      autoCommit() { implicit session =>
        sqlutil.executeStatements(
          s"""CREATE USER '$userName'@'%' IDENTIFIED BY '$password';
             |GRANT ALL ON $observeeDbName.*  TO '$userName'@'%';
             |GRANT ALL ON $changeLogDbName.* TO '$userName'@'%';""".stripMargin
        )
      }
      test
    }
  }

  protected[this] def withTableUserAndDatabases(test: => Any) {
    withUserAndDatabases {
      autoCommit(defaultDbConnectionPool) { implicit session =>
        (s"CREATE TABLE $observeeDbName.table1(pr1 INTEGER not null," +
          "pr2 VARCHAR(30) not null, col1 INTEGER, PRIMARY KEY(pr1, pr2))").query
      }
      test
    }
  }

}
