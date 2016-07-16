package com.github.bigwheel.changerecorder

import scalikejdbc._

class ChangeRecorder private(observeePool: ConnectionPool, recordPool: ConnectionPool) {
  def this(observeeDbUrl: String, recordDbUrl: String, user: String, password: String) = this(
    Commons2ConnectionPoolFactory(observeeDbUrl, user, password),
    Commons2ConnectionPoolFactory(recordDbUrl, user, password)
  )

  def prepare() = {
    using(DB(observeePool.borrow)) { observeeDb =>
      observeeDb.autoClose(false) // loanパターン使うときはautoCloseは自動でOFFにしてくれないものか…
      using(DB(recordPool.borrow)) { recordDb =>
        recordDb.autoClose(false) // loanパターン使うときはautoCloseは自動でOFFにしてくれないものか…
        recordDb.autoCommit { implicit session =>
          for (tableName <- observeeDb.getTableNames()) {
            val primaryColumnNames = observeeDb.getTable(tableName).get.columns.
              withFilter(_.isPrimaryKey).map(_.name).mkString(", ")
            SQL(s"CREATE TABLE $tableName (PRIMARY KEY($primaryColumnNames)) AS " +
              s"SELECT $primaryColumnNames FROM observee.$tableName WHERE FALSE").execute.apply()
          }
        }
      }
    }
  }
}
