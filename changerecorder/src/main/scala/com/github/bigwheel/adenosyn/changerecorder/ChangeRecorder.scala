package com.github.bigwheel.adenosyn.changerecorder

import scalikejdbc._

class ChangeRecorder private(observeeDbName: String, recordDbName: String,
  observeePool: ConnectionPool, recordPool: ConnectionPool) {
  def this(observeeDbUrl: JdbcUrl, recordDbUrl: JdbcUrl, user: String, password: String) = this(
    observeeDbUrl.dbName, recordDbUrl.dbName,
    Commons2ConnectionPoolFactory(observeeDbUrl.plainUrl, user, password),
    Commons2ConnectionPoolFactory(recordDbUrl.plainUrl, user, password)
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

            SQL( // 監視対象のテーブルと全く同じカラム定義になるようCREATE TABLE ... AS SELECT文を使う
              s"CREATE TABLE IF NOT EXISTS $tableName (PRIMARY KEY($primaryColumnNames)) AS " +
              s"SELECT $primaryColumnNames FROM $observeeDbName.$tableName WHERE FALSE"
            ).execute.apply()

            observeeDb.autoCommit { implicit session =>
              // OLD.とNEW.はtrigger文固有のキーワード
              // https://dev.mysql.com/doc/refman/5.6/ja/trigger-syntax.html
              val queries = Seq(
                s"""CREATE TRIGGER changerecorder_observee_${tableName}_insert AFTER INSERT
                    |ON $observeeDbName.$tableName FOR EACH ROW
                    |REPLACE INTO $recordDbName.$tableName($primaryColumnNames)
                    |VALUES(${primaryColumnNameList.map("NEW." + _).mkString(", ")})""",
                s"""CREATE TRIGGER changerecorder_observee_${tableName}_update AFTER UPDATE
                    |ON $observeeDbName.$tableName FOR EACH ROW
                    |REPLACE INTO $recordDbName.$tableName($primaryColumnNames)
                    |VALUES
                    |(${primaryColumnNameList.map("OLD." + _).mkString(", ")}),
                    |(${primaryColumnNameList.map("NEW." + _).mkString(", ")})""",
                s"""CREATE TRIGGER changerecorder_observee_${tableName}_delete AFTER DELETE
                    |ON $observeeDbName.$tableName FOR EACH ROW
                    |REPLACE INTO $recordDbName.$tableName($primaryColumnNames)
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
          s"WHERE TRIGGER_SCHEMA = '$observeeDbName'"
        val triggerNames = SQL(q).map(_.string(1)).list.apply()
        for (triggerName <- triggerNames)
          SQL(s"DROP TRIGGER $observeeDbName.$triggerName").execute.apply()
      }
    }
  }

}
