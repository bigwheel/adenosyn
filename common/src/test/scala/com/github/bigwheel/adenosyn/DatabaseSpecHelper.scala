package com.github.bigwheel.adenosyn

import com.github.bigwheel.adenosyn.sqlutil._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.Suite
import scalikejdbc._

trait DatabaseSpecHelper extends BeforeAndAfterAll { this: Suite =>

  Class.forName("com.mysql.jdbc.Driver")

  // http://qiita.com/suin/items/5a7a56afacc8a35abcb6
  private[this] def md5(text: String): String = java.security.MessageDigest.
    getInstance("MD5").digest(text.getBytes).map("%02x".format(_)).mkString

  protected[this] val postfix = md5(this.getClass().getCanonicalName()).take(8)
  protected[this] val observeeDbName = "observee" + postfix
  protected[this] val changeLogDbName = "changelog" + postfix
  protected[this] val userName = "changeloggermanager" + postfix
  protected[this] val password = "clm" + postfix

  override protected def beforeAll() = {
    super.beforeAll()

    ConnectionPool.singleton(sqlutil.url(), "root", "root")
    ConnectionPool.add('observee, sqlutil.url(observeeDbName), "root", "root")
    ConnectionPool.add('changelog, sqlutil.url(changeLogDbName), "root", "root")
  }

  protected[this] implicit val session = AutoSession

  protected[this] def withDatabases(test: => Any) {
    sqlutil.executeStatements(
      s"""DROP DATABASE IF EXISTS $observeeDbName;
         |CREATE DATABASE $observeeDbName;
         |DROP DATABASE IF EXISTS $changeLogDbName;
         |CREATE DATABASE $changeLogDbName;
         |DROP USER IF EXISTS '$userName'@'%';""".stripMargin
    )
    test
  }

  protected[this] def withUserAndDatabases(test: => Any) {
    withDatabases {
      sqlutil.executeStatements(
        s"""CREATE USER '$userName'@'%' IDENTIFIED BY '$password';
           |GRANT ALL ON $observeeDbName.* TO '$userName'@'%';
           |GRANT ALL ON $changeLogDbName.* TO '$userName'@'%';""".stripMargin
      )
      test
    }
  }

  protected[this] def withTableUserAndDatabases(test: => Any) {
    withUserAndDatabases {
      (s"CREATE TABLE $observeeDbName.table1(pr1 INTEGER not null," +
        "pr2 VARCHAR(30) not null, col1 INTEGER, PRIMARY KEY(pr1, pr2))").query
      test
    }
  }

}
