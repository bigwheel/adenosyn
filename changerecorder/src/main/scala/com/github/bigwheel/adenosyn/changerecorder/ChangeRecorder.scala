package com.github.bigwheel.adenosyn.changerecorder

import scalikejdbc._

class ChangeRecorder private(observeeDbName: String, recordDbName: String,
  connectionPool: ConnectionPool) {

  def this(jdbcUrl: String, observeeDbName: String, recordDbName: String, username: String,
    password: String) = this(observeeDbName, recordDbName,
    Commons2ConnectionPoolFactory(jdbcUrl, username, password)
  )

  override def finalize(): Unit = connectionPool.close

  def setUp() = {
    val tableNameAndPrimaryColumnsList = using(DB(connectionPool.borrow)) { db =>
      db.autoClose(false)
      // http://stackoverflow.com/a/13433382/4006322
      db.conn.setCatalog(observeeDbName)
      for (tableName <- db.getTableNames())
        yield (tableName, db.getTable(tableName).get.columns.filter(_.isPrimaryKey))
    }

    val queries = tableNameAndPrimaryColumnsList.map { case (tableName, primaryColumns) =>
      val primaryColumnNameList = primaryColumns.map(_.name)
      val primaryColumnNames = primaryColumnNameList.mkString(", ")

      val queryForRecord =
        s"CREATE TABLE IF NOT EXISTS $recordDbName.$tableName " +
          s"(PRIMARY KEY($primaryColumnNames)) AS " +
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

    using(DB(connectionPool.borrow)) { db =>
      db.autoCommit { implicit session =>
        queriesForObservee.foreach(SQL(_).execute.apply())
      }
    }

    using(DB(connectionPool.borrow)) { db =>
      db.autoCommit { implicit session =>
        queriesForRerord.foreach(SQL(_).execute.apply())
      }
    }

  }

  def tearDown() = {
    val queries = using(DB(connectionPool.borrow)) { db =>
      db.readOnly { implicit session =>
        val q = "SELECT TRIGGER_NAME FROM information_schema.triggers " +
          s"WHERE TRIGGER_SCHEMA = '$observeeDbName'"
        val triggerNames = SQL(q).map(_.string(1)).list.apply()
        for (triggerName <- triggerNames)
          yield s"DROP TRIGGER $observeeDbName.$triggerName"
      }
    }

    using(DB(connectionPool.borrow)) { db =>
      db.autoCommit { implicit session =>
        queries.foreach(SQL(_).execute.apply())
      }
    }
  }

}
