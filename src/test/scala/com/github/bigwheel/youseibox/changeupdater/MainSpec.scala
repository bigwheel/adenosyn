package com.github.bigwheel.youseibox.changeupdater

import com.github.bigwheel.youseibox.sqlutil
import org.scalatest.FunSpec
import org.scalatest.Matchers
import scalikejdbc.ConnectionPool
import scalikejdbc.DB
import scalikejdbc.NamedDB

class MainSpec extends FunSpec with Matchers {

  sqlutil.suppressLog()

  Class.forName("com.mysql.jdbc.Driver")
  ConnectionPool.singleton(sqlutil.url(), "root", "root")
  ConnectionPool.add('observee, sqlutil.url("observee"), "root", "root")
  ConnectionPool.add('record, sqlutil.url("record"), "root", "root")

  private[this] def withDatabases(test: => Any) {
    DB.autoCommit { implicit session =>
      sqlutil.executeStatements(
        """DROP DATABASE IF EXISTS observee;
          |CREATE DATABASE         observee;
          |DROP DATABASE IF EXISTS record;
          |CREATE DATABASE         record;
          |DROP USER IF EXISTS 'youseibox'@'%';
          |CREATE USER         'youseibox'@'%' IDENTIFIED BY 'youseibox';
          |GRANT ALL ON observee.* TO 'youseibox'@'%';
          |GRANT ALL ON record.*   TO 'youseibox'@'%';""".stripMargin
      )
    }
    NamedDB('observee).autoCommit { implicit session =>
      sqlutil.executeScript("/fixture.sql")
    }

    test
  }

  it("a") {
    withDatabases {
      noException should be thrownBy {
        println("test")
      }
    }
  }
}
