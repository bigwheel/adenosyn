package com.github.bigwheel.adenosyn.changerecorder

import com.github.bigwheel.adenosyn.sqlutil
import com.github.bigwheel.adenosyn.sqlutil._
import org.scalatest.FunSpec
import org.scalatest.Matchers
import scalikejdbc._
import scalikejdbc.metadata.Column

class MainSpec extends FunSpec with Matchers {

  sqlutil.suppressLog()

  {
    Class.forName("com.mysql.jdbc.Driver")
    ConnectionPool.singleton(sqlutil.url(), "root", "root")
    ConnectionPool.add('observee, sqlutil.url("observee"), "root", "root")
    ConnectionPool.add('record, sqlutil.url("record"), "root", "root")
  }

  private[this] implicit val session = AutoSession

  def withDatabases(test: => Any) {
    sqlutil.executeStatements(
      """DROP DATABASE IF EXISTS observee;
        |CREATE DATABASE observee;
        |DROP DATABASE IF EXISTS record;
        |CREATE DATABASE record;
        |DROP USER IF EXISTS 'changerecorder'@'%';""".stripMargin
    )
    test
  }

  def withUserAndDatabases(test: => Any) {
    withDatabases {
      sqlutil.executeStatements(
        """CREATE USER 'changerecorder'@'%' IDENTIFIED BY 'cr';
          |GRANT ALL ON observee.* TO 'changerecorder'@'%';
          |GRANT ALL ON record.* TO 'changerecorder'@'%';""".stripMargin
      )
      test
    }
  }

  def withTableUserAndDatabases(test: => Any) {
    withUserAndDatabases {
      ("CREATE TABLE observee.table1(pr1 INTEGER not null," +
        "pr2 VARCHAR(30) not null, col1 INTEGER, PRIMARY KEY(pr1, pr2))").query
      test
    }
  }

  describe(".setUp") {
    // TODO: 例外を厳密に指定する
    it("監視対象データベースに監視ユーザーが存在しないと例外が出る") {
      withDatabases {
        a[Exception] should be thrownBy {
          new ChangeRecorder(jdbcUrl("observee"), jdbcUrl("record"), "changerecorder", "cr").setUp
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
          new ChangeRecorder(jdbcUrl("observee"), jdbcUrl("record"), "changerecorder", "cr").setUp
        }
      }
    }

    it("適切な権限があれば成功する") {
      withUserAndDatabases {
        noException should be thrownBy {
          new ChangeRecorder(jdbcUrl("observee"), jdbcUrl("record"), "changerecorder", "cr").setUp
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

        new ChangeRecorder(jdbcUrl("observee"), jdbcUrl("record"), "changerecorder", "cr").setUp

        NamedDB('record).getTable("table1").nonEmpty should be(true)
      }
    }

    describe("primary keyだけのテーブルができているかチェック") {

      it("primary keyではないカラムがあっても記録データベース側のテーブルにはprimary keyカラムしかない") {
        withTableUserAndDatabases {
          new ChangeRecorder(jdbcUrl("observee"), jdbcUrl("record"), "changerecorder", "cr").setUp

          NamedDB('record).getTable("table1").get.columns.withFilter(_.isPrimaryKey).
            map(_.name) should be(List("pr1", "pr2"))
        }
      }

      it("カラム定義が監視対象データベースと一致する") {
        withTableUserAndDatabases {
          new ChangeRecorder(jdbcUrl("observee"), jdbcUrl("record"), "changerecorder", "cr").setUp

          NamedDB('record).getTable("table1").get.columns should matchPattern {
            case List(Column("pr1", _, "INT", _, true, true, false, _, _),
            Column("pr2", _, "VARCHAR", 30, true, true, false, _, _)) =>
          }
        }
      }

      it("行がある状態で実行しても記録テーブルは空") {
        withTableUserAndDatabases {
          "INSERT INTO observee.table1 (pr1, pr2, col1) VALUES (1, 'test', 3)".query

          new ChangeRecorder(jdbcUrl("observee"), jdbcUrl("record"), "changerecorder", "cr").setUp

          NamedDB('record).readOnly { implicit session =>
            SQL("SELECT COUNT(*) FROM table1").map(_.int(1)).single.apply().get should be(0)
          }
        }
      }

      describe(".setUp実行後") {

        it("監視対象テーブルへ行を追加すると記録テーブルに対応する行ができる") {
          withTableUserAndDatabases {
            new ChangeRecorder(jdbcUrl("observee"), jdbcUrl("record"), "changerecorder", "cr").setUp

            "INSERT INTO observee.table1 (pr1, pr2, col1) VALUES (1, 'test', 3)".query

            NamedDB('record).readOnly { implicit session =>
              SQL("SELECT COUNT(*) FROM table1").map(_.int(1)).single.apply().get should be(1)
            }
          }
        }

        it("監視対象テーブルの行を削除すると記録テーブルに対応する行ができる") {
          withTableUserAndDatabases {
            "INSERT INTO observee.table1 (pr1, pr2, col1) VALUES (1, 'test', 3)".query

            new ChangeRecorder(jdbcUrl("observee"), jdbcUrl("record"), "changerecorder", "cr").setUp

            "DELETE FROM observee.table1".query

            NamedDB('record).readOnly { implicit session =>
              SQL("SELECT COUNT(*) FROM table1").map(_.int(1)).single.apply().get should be(1)
            }
          }
        }

        it("Primary Keyのカラムが更新されると記録テーブルに対応する2行ができる") {
          withTableUserAndDatabases {
            "INSERT INTO observee.table1 (pr1, pr2, col1) VALUES (1, 'test', 3)".query

            new ChangeRecorder(jdbcUrl("observee"), jdbcUrl("record"), "changerecorder", "cr").setUp

            "UPDATE observee.table1 SET pr1=2".query

            NamedDB('record).readOnly { implicit session =>
              SQL("SELECT COUNT(*) FROM table1").map(_.int(1)).single.apply().get should be(2)
            }
          }
        }

        it("Primary Key以外のカラムが更新されると記録テーブルに対応する1行ができる") {
          withTableUserAndDatabases {
            "INSERT INTO observee.table1 (pr1, pr2, col1) VALUES (1, 'test', 3)".query

            new ChangeRecorder(jdbcUrl("observee"), jdbcUrl("record"), "changerecorder", "cr").setUp

            "UPDATE observee.table1 SET col1=2".query

            NamedDB('record).readOnly { implicit session =>
              SQL("SELECT COUNT(*) FROM table1").map(_.int(1)).single.apply().get should be(1)
            }
          }
        }

      }
    }
  }

  describe(".tearDown") {

    it("setUp後にtearDownすると監視対象テーブルへ行を追加しても記録テーブルへは行が追加されない") {
      withTableUserAndDatabases {
        noException should be thrownBy {
          val cr = new ChangeRecorder(jdbcUrl("observee"), jdbcUrl("record"), "changerecorder", "cr")
          cr.setUp
          cr.tearDown

          "INSERT INTO observee.table1 (pr1, pr2, col1) VALUES (1, 'test', 3)".query

          NamedDB('record).readOnly { implicit session =>
            SQL("SELECT COUNT(*) FROM table1").map(_.int(1)).single.apply().get should be(0)
          }
        }
      }
    }

    it("setUp後にtearDown、さらにsetUpしても問題ない") {
      withTableUserAndDatabases {
        noException should be thrownBy {
          val cr = new ChangeRecorder(jdbcUrl("observee"), jdbcUrl("record"), "changerecorder", "cr")
          cr.setUp
          cr.tearDown
          cr.setUp
        }
      }
    }

  }

}
