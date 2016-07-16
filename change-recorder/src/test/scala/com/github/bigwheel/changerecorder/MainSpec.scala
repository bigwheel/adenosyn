package com.github.bigwheel.changerecorder

import org.scalatest.FunSpec
import org.scalatest.Matchers
import scala.sys.process.Process
import scalikejdbc.ConnectionPool
import scalikejdbc.DB
import scalikejdbc.NamedDB
import scalikejdbc.SQL
import scalikejdbc.metadata.Column

class MainSpec extends FunSpec with Matchers {

  def url(dbName: String = "") = {
    val ipAddress = Process("otto dev address").!!.stripLineEnd
    s"jdbc:mysql://$ipAddress/$dbName?characterEncoding=UTF-8&useSSL=false"
  }

  private[this] implicit class RichString(q: String) {
    def query() = DB.autoCommit { implicit session => SQL(q).execute.apply() }
  }

  private[this] implicit class RichSeqString(queries: Seq[String]) {
    def query() = DB.autoCommit { implicit session => queries.foreach(SQL(_).execute.apply()) }
  }

  {
    Class.forName("com.mysql.jdbc.Driver")
    ConnectionPool.singleton(url(), "root", "root")
    ConnectionPool.add('observee, url("observee"), "root", "root")
    ConnectionPool.add('record, url("record"), "root", "root")
  }

  def withDatabases(test: => Any) {
    Seq(
      "DROP DATABASE IF EXISTS observee",
      "CREATE DATABASE observee",
      "DROP DATABASE IF EXISTS record",
      "CREATE DATABASE record",
      "DROP USER IF EXISTS 'changerecorder'@'%'"
    ).query
    test
  }

  def withUserAndDatabases(test: => Any) {
    withDatabases {
      Seq(
        "CREATE USER 'changerecorder'@'%' IDENTIFIED BY 'changerecorder'",
        "GRANT ALL ON observee.* TO 'changerecorder'@'%'",
        "GRANT ALL ON record.* TO 'changerecorder'@'%'"
      ).query
      test
    }
  }

  describe(".prepare") {
    // TODO: 例外を厳密に指定する
    it("監視対象データベースに監視ユーザーが存在しないと例外が出る") {
      withDatabases {
        a[Exception] should be thrownBy {
          new ChangeRecorder(url("observee"), url("record"), "changerecorder", "changerecorder").
            prepare
        }
      }
    }

    // TODO: 例外を厳密に指定する
    it("記録データベースに監視ユーザーが存在しないと例外が出る") {
      withDatabases {
        Seq(
          "CREATE USER 'changerecorder'@'%' IDENTIFIED BY 'changerecorder'",
          "GRANT ALL ON observee.* TO 'changerecorder'@'%'"
        ).query

        a[Exception] should be thrownBy {
          new ChangeRecorder(url("observee"), url("record"), "changerecorder", "changerecorder").
            prepare
        }
      }
    }

    it("適切な権限があれば成功する") {
      withDatabases {
        Seq(
          "CREATE USER 'changerecorder'@'%' IDENTIFIED BY 'changerecorder'",
          "GRANT ALL ON observee.* TO 'changerecorder'@'%'",
          "GRANT ALL ON record.* TO 'changerecorder'@'%'"
        ).query

        noException should be thrownBy {
          new ChangeRecorder(url("observee"), url("record"), "changerecorder", "changerecorder").
            prepare
        }
      }
    }

    // TODO: 例外を厳密に指定する
    it("権限が不足していると例外が出る") {
      withDatabases {
        pending
      }
    }

    // できればログに出力されたことを確認したいが結構工夫が必要そう
    it("権限が不要に大きいと警告を出す") {
      withDatabases {
        pending
      }
    }

    it("監視対象データベースに対応したテーブルが作られる") {
      withUserAndDatabases {
        "CREATE TABLE observee.table1(id INTEGER PRIMARY KEY not null)".query

        new ChangeRecorder(url("observee"), url("record"), "changerecorder", "changerecorder").
          prepare

        NamedDB('record).getTable("table1").nonEmpty should be(true)
      }
    }

    describe("primary keyだけのテーブルができているかチェック") {
      it("primary keyではないカラムがあっても記録データベース側のテーブルにはprimary keyカラムしかない") {
        withUserAndDatabases {
          "CREATE TABLE observee.table1(pr1 INTEGER PRIMARY KEY not null, col1 INTEGER)".query

          new ChangeRecorder(url("observee"), url("record"), "changerecorder", "changerecorder").
            prepare

          NamedDB('record).getTable("table1").get.columns.withFilter(_.isPrimaryKey).
            map(_.name) should be(List("pr1"))
        }
      }

      it("カラム定義が監視対象データベースと一致する") {
        withUserAndDatabases {
          ("CREATE TABLE observee.table1(" +
            "pr1 INTEGER not null," +
            "pr2 VARCHAR(30) not null," +
            "col1 INTEGER, PRIMARY KEY(pr1, pr2))").query

          new ChangeRecorder(url("observee"), url("record"), "changerecorder", "changerecorder").
            prepare

          NamedDB('record).getTable("table1").get.columns should matchPattern {
            case List(Column("pr1", _, "INT", _, true, true, false, _, _),
            Column("pr2", _, "VARCHAR", 30, true, true, false, _, _)) =>
          }
        }
      }
    }
  }

}
