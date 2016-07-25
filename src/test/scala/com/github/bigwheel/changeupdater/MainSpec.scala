package com.github.bigwheel.changeupdater

import com.github.bigwheel.util
import org.scalatest.FunSpec
import org.scalatest.Matchers
import scalikejdbc.ConnectionPool
import scalikejdbc.DB
import scalikejdbc.NamedDB

class MainSpec extends FunSpec with Matchers {

  util.suppressSqlLog()

  Class.forName("com.mysql.jdbc.Driver")
  ConnectionPool.singleton(util.url(), "root", "root")
  ConnectionPool.add('observee, util.url("observee"), "root", "root")
  ConnectionPool.add('record, util.url("record"), "root", "root")

  private[this] def withDatabases(test: => Any) {
    DB.autoCommit { implicit session =>
      util.executeSqlStatements(
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
      util.executeSqlScript("/fixture.sql")
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
