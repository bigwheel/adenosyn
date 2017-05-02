package com.github.bigwheel.adenosyn.changerecorder

import com.github.bigwheel.adenosyn.sqlutil
import com.github.bigwheel.adenosyn.sqlutil._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.slf4j.LoggerFactory
import scala.sys.process.Process
import scala.sys.process.ProcessLogger
import scalikejdbc._
import scalikejdbc.metadata.Column

class ChangeRecorderSpec extends FreeSpec with Matchers with BeforeAndAfterAll {

  private[this] val postfix = "398"
  private[this] val observeeDbName = "observee" + postfix
  private[this] val recordDbName = "record" + postfix
  private[this] val userName = "changerecorder" + postfix
  private[this] val password = "cr" + postfix

  override protected def beforeAll() = {
    val l = LoggerFactory.getLogger(getClass)
    Process("docker-compose up -d").!(ProcessLogger(l.debug, l.warn))

    Class.forName("com.mysql.jdbc.Driver")
    ConnectionPool.singleton(sqlutil.url(), "root", "root")
    ConnectionPool.add('observee, sqlutil.url(observeeDbName), "root", "root")
    ConnectionPool.add('record, sqlutil.url(recordDbName), "root", "root")
  }

  private[this] implicit val session = AutoSession

  private[this] def withDatabases(test: => Any) {
    sqlutil.executeStatements(
      s"""DROP DATABASE IF EXISTS $observeeDbName;
        |CREATE DATABASE $observeeDbName;
        |DROP DATABASE IF EXISTS $recordDbName;
        |CREATE DATABASE $recordDbName;
        |DROP USER IF EXISTS '$userName'@'%';""".stripMargin
    )
    test
  }

  private[this] def withUserAndDatabases(test: => Any) {
    withDatabases {
      sqlutil.executeStatements(
        s"""CREATE USER '$userName'@'%' IDENTIFIED BY '$password';
          |GRANT ALL ON $observeeDbName.* TO '$userName'@'%';
          |GRANT ALL ON $recordDbName.* TO '$userName'@'%';""".stripMargin
      )
      test
    }
  }

  private[this] def withTableUserAndDatabases(test: => Any) {
    withUserAndDatabases {
      (s"CREATE TABLE $observeeDbName.table1(pr1 INTEGER not null," +
        "pr2 VARCHAR(30) not null, col1 INTEGER, PRIMARY KEY(pr1, pr2))").query
      test
    }
  }

  private[this] def subject = new ChangeRecorder(jdbcUrlForTest(observeeDbName),
    jdbcUrlForTest(recordDbName), userName, password)

  ".setUp" - {
    "database permission aspect:" - {
      "there is no user in observee database" - {
        "produce Exception" in {
          withDatabases {
            a[Exception] should be thrownBy {
              subject.setUp
            }
          }
        }
      }

      "there is the user in observee database" - {
        "there is no user in record database" - {
          "produce Exception" in {
            withDatabases {
              Seq(s"CREATE USER '$userName'@'%' IDENTIFIED BY 'changerecorder'",
                s"GRANT ALL ON $observeeDbName.* TO '$userName'@'%'").query

              a[Exception] should be thrownBy {
                subject.setUp
              }
            }
          }
        }

        "there is the user in record database" - {
          "succeed with appropriate permission" in {
            withUserAndDatabases {
              noException should be thrownBy {
                subject.setUp
              }
            }
          }

          "produce Exception with insufficient permission" in {
            withDatabases {
              pending
            }
          }

          // It is hard to check that there is warn log output
          "produce warn log with excessive permission" in {
            withDatabases {
              pending
            }
          }
        }
      }
    }

    "record table definition aspect:" - {

      "create a record table in record database" in {
        withUserAndDatabases {
          s"CREATE TABLE $observeeDbName.table1(id INTEGER PRIMARY KEY not null)".query

          subject.setUp

          NamedDB('record).getTable("table1") shouldNot be(empty)
        }
      }

      "the record table has only primary keys of observee table" in {
        withTableUserAndDatabases {
          subject.setUp

          NamedDB('record).getTable("table1").get.columns.withFilter(_.isPrimaryKey).
            map(_.name) should be(List("pr1", "pr2"))
        }
      }

      "column definitions of primary keys in record table match it in observee table" in {
        withTableUserAndDatabases {
          subject.setUp

          NamedDB('record).getTable("table1").get.columns should matchPattern {
            case List(Column("pr1", _, "INT", _, true, true, false, _, _),
            Column("pr2", _, "VARCHAR", 30, true, true, false, _, _)) =>
          }
        }
      }
    }

    "record manipulation aspect:" - {
      "no rows created in record table when a row is inserted to observee table before .setUp is executed" in {
        withTableUserAndDatabases {
          s"INSERT INTO $observeeDbName.table1 (pr1, pr2, col1) VALUES (1, 'test', 3)".query

          subject.setUp

          NamedDB('record).readOnly { implicit session =>
            SQL("SELECT COUNT(*) FROM table1").map(_.int(1)).single.apply().get should be(0)
          }
        }
      }

      "the row created in record table when a row is inserted to observee table after .setUp is executed" in {
        withTableUserAndDatabases {
          subject.setUp

          s"INSERT INTO $observeeDbName.table1 (pr1, pr2, col1) VALUES (1, 'test', 3)".query

          NamedDB('record).readOnly { implicit session =>
            SQL("SELECT COUNT(*) FROM table1").map(_.int(1)).single.apply().get should be(1)
          }
        }
      }

      "the row created in record table when a row is deleted to observee table after .setUp is executed" in {
        withTableUserAndDatabases {
          s"INSERT INTO $observeeDbName.table1 (pr1, pr2, col1) VALUES (1, 'test', 3)".query

          subject.setUp

          s"DELETE FROM $observeeDbName.table1".query

          NamedDB('record).readOnly { implicit session =>
            SQL("SELECT COUNT(*) FROM table1").map(_.int(1)).single.apply().get should be(1)
          }
        }
      }

      "the row created in record table when a primary key of a row is updated to observee table after .setUp is executed" in {
        withTableUserAndDatabases {
          s"INSERT INTO $observeeDbName.table1 (pr1, pr2, col1) VALUES (1, 'test', 3)".query

          subject.setUp

          s"UPDATE $observeeDbName.table1 SET pr1=2".query

          NamedDB('record).readOnly { implicit session =>
            SQL("SELECT COUNT(*) FROM table1").map(_.int(1)).single.apply().get should be(2)
          }
        }
      }

      "the row created in record table when a non primary key of a row is updated to observee table after .setUp is executed" in {
        withTableUserAndDatabases {
          s"INSERT INTO $observeeDbName.table1 (pr1, pr2, col1) VALUES (1, 'test', 3)".query

          subject.setUp

          s"UPDATE $observeeDbName.table1 SET col1=2".query

          NamedDB('record).readOnly { implicit session =>
            SQL("SELECT COUNT(*) FROM table1").map(_.int(1)).single.apply().get should be(1)
          }
        }
      }

    }
  }

  ".tearDown" - {

    "record manipulation aspect:" - {
      "no rows created in record table when a row is inserted to observee table after .tearDown is executed" in {
        withTableUserAndDatabases {
          noException should be thrownBy {
            val subj = subject
            subj.setUp
            subj.tearDown

            s"INSERT INTO $observeeDbName.table1 (pr1, pr2, col1) VALUES (1, 'test', 3)".query

            NamedDB('record).readOnly { implicit session =>
              SQL("SELECT COUNT(*) FROM table1").map(_.int(1)).single.apply().get should be(0)
            }
          }
        }
      }

      "no Exception in .setUp after .tearDown" in {
        withTableUserAndDatabases {
          noException should be thrownBy {
            val subj = subject
            subj.setUp
            subj.tearDown
            subj.setUp
          }
        }
      }

    }
  }

}
