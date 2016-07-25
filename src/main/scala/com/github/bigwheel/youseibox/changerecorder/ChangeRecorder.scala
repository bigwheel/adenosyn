package com.github.bigwheel.youseibox.changerecorder

import scalikejdbc._

class ChangeRecorder private(observeePool: ConnectionPool, recordPool: ConnectionPool) {
  def this(observeeDbUrl: String, recordDbUrl: String, user: String, password: String) = this(
    Commons2ConnectionPoolFactory(observeeDbUrl, user, password),
    Commons2ConnectionPoolFactory(recordDbUrl, user, password)
  )

  def setUp() = {
    using(DB(observeePool.borrow)) { observeeDb =>
      observeeDb.autoClose(false) // loanパターン使うときはautoCloseは自動でOFFにしてくれないものか…
      using(DB(recordPool.borrow)) { recordDb =>
        recordDb.autoClose(false) // loanパターン使うときはautoCloseは自動でOFFにしてくれないものか…
        recordDb.autoCommit { implicit session =>
          for (tableName <- observeeDb.getTableNames()) {
            val primaryColumnNameList = observeeDb.getTable(tableName).get.columns.
              withFilter(_.isPrimaryKey).map(_.name)
            val primaryColumnNames = primaryColumnNameList.mkString(", ")

            SQL(s"CREATE TABLE IF NOT EXISTS $tableName (PRIMARY KEY($primaryColumnNames)) AS " +
              s"SELECT $primaryColumnNames FROM observee.$tableName WHERE FALSE").execute.apply()

            observeeDb.autoCommit { implicit session =>
              val queries = Seq(
                s"""CREATE TRIGGER changerecorder_observee_${tableName}_insert AFTER INSERT
                    |ON observee.$tableName FOR EACH ROW
                    |REPLACE INTO record.$tableName($primaryColumnNames)
                    |VALUES(${primaryColumnNameList.map("NEW." + _).mkString(", ")})""",
                s"""CREATE TRIGGER changerecorder_observee_${tableName}_update AFTER UPDATE
                    |ON observee.$tableName FOR EACH ROW
                    |REPLACE INTO record.$tableName($primaryColumnNames)
                    |VALUES
                    |(${primaryColumnNameList.map("OLD." + _).mkString(", ")}),
                    |(${primaryColumnNameList.map("NEW." + _).mkString(", ")})""",
                s"""CREATE TRIGGER changerecorder_observee_${tableName}_delete AFTER DELETE
                    |ON observee.$tableName FOR EACH ROW
                    |REPLACE INTO record.$tableName($primaryColumnNames)
                    |VALUES(${primaryColumnNameList.map("OLD." + _).mkString(", ")})"""
              )
              queries.map(_.stripMargin.replace('\n', ' ')).foreach(SQL(_).execute.apply())
            }
          }
        }
      }
    }
  }

  def tearDown() = {
    using(DB(observeePool.borrow)) { observeeDb =>
      observeeDb.autoCommit { implicit session =>
        val q = "SELECT TRIGGER_NAME FROM information_schema.triggers " +
          "WHERE TRIGGER_SCHEMA = 'observee'"
        val triggerNames = SQL(q).map(_.string(1)).list.apply()
        for (triggerName <- triggerNames)
          SQL(s"DROP TRIGGER observee.$triggerName").execute.apply()
      }
    }
  }

}
