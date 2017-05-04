package com.github.bigwheel.adenosyn.changerecorder

import com.github.bigwheel.adenosyn.JdbcUrl
import scalikejdbc._

class ChangeRecorder private(observeeDbName: String, recordDbName: String,
  observeePool: ConnectionPool, recordPool: ConnectionPool) {
  def this(observeeDbUrl: JdbcUrl, recordDbUrl: JdbcUrl, user: String, password: String) = this(
    observeeDbUrl.dbName, recordDbUrl.dbName,
    Commons2ConnectionPoolFactory(observeeDbUrl.plainUrl, user, password),
    Commons2ConnectionPoolFactory(recordDbUrl.plainUrl, user, password)
  )

  override def finalize(): Unit = {
    observeePool.close
    recordPool.close
  }

  def setUp() = {
    val tableNameAndPrimaryColumnsList = using(DB(observeePool.borrow)) { observeeDb =>
      observeeDb.autoClose(false)
      for (tableName <- observeeDb.getTableNames())
        yield (tableName, observeeDb.getTable(tableName).get.columns.filter(_.isPrimaryKey))
    }

    val queries = tableNameAndPrimaryColumnsList.map { case (tableName, primaryColumns) =>
      val primaryColumnNameList = primaryColumns.map(_.name)
      val primaryColumnNames = primaryColumnNameList.mkString(", ")

      val queryForRecord =
        s"CREATE TABLE IF NOT EXISTS $tableName (PRIMARY KEY($primaryColumnNames)) AS " +
          s"SELECT $primaryColumnNames FROM $observeeDbName.$tableName WHERE FALSE"

      // OLD.とNEW.はtrigger文固有のキーワード
      // https://dev.mysql.com/doc/refman/5.6/ja/trigger-syntax.html
      val queriesForObservee = Seq(
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
      ).map(_.stripMargin.replace('\n', ' '))

      (queriesForObservee, queryForRecord)
    }

    val temp = queries.unzip
    val queriesForObservee = temp._1.flatten
    val queriesForRerord = temp._2

    using(DB(observeePool.borrow)) { observeeDb =>
      observeeDb.autoCommit { implicit session =>
        queriesForObservee.foreach(SQL(_).execute.apply())
      }
    }

    using(DB(recordPool.borrow)) { recordDb =>
      recordDb.autoCommit { implicit session =>
        queriesForRerord.foreach(SQL(_).execute.apply())
      }
    }

  }

  def tearDown() = {
    val queries = using(DB(observeePool.borrow)) { observeeDb =>
      observeeDb.readOnly { implicit session =>
        val q = "SELECT TRIGGER_NAME FROM information_schema.triggers " +
          s"WHERE TRIGGER_SCHEMA = '$observeeDbName'"
        val triggerNames = SQL(q).map(_.string(1)).list.apply()
        for (triggerName <- triggerNames)
          yield s"DROP TRIGGER $observeeDbName.$triggerName"
      }
    }

    using(DB(observeePool.borrow)) { observeeDb =>
      observeeDb.autoCommit { implicit session =>
        queries.foreach(SQL(_).execute.apply())
      }
    }
  }

}
