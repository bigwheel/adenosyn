package com.github.bigwheel.adenosyn.changeloggermanager

import scalikejdbc._

case class SetupQueries(forObservee: List[String], forChangeLog: List[String])

class ChangeLoggerManager private(observeeDbName: String, changeLogDbName: String,
  connectionPool: ConnectionPool) {

  def this(jdbcUrl: String, observeeDbName: String, changeLogDbName: String, username: String,
    password: String) = this(observeeDbName, changeLogDbName,
    Commons2ConnectionPoolFactory(jdbcUrl, username, password)
  )

  override def finalize(): Unit = connectionPool.close

  lazy val setUpQueries: SetupQueries = {
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

      // 'OLD' and 'NEW' are special keyword of trigger
      // https://dev.mysql.com/doc/refman/5.6/ja/trigger-syntax.html
      val changeLogger = Seq(
        s"""CREATE TRIGGER changeloggermanager_observee_${tableName}_insert AFTER INSERT
           |ON $observeeDbName.$tableName FOR EACH ROW
           |REPLACE INTO $changeLogDbName.$tableName($primaryColumnNames)
           |VALUES(${primaryColumnNameList.map("NEW." + _).mkString(", ")})""",
        s"""CREATE TRIGGER changeloggermanager_observee_${tableName}_update AFTER UPDATE
           |ON $observeeDbName.$tableName FOR EACH ROW
           |REPLACE INTO $changeLogDbName.$tableName($primaryColumnNames)
           |VALUES
           |(${primaryColumnNameList.map("OLD." + _).mkString(", ")}),
           |(${primaryColumnNameList.map("NEW." + _).mkString(", ")})""",
        s"""CREATE TRIGGER changeloggermanager_observee_${tableName}_delete AFTER DELETE
           |ON $observeeDbName.$tableName FOR EACH ROW
           |REPLACE INTO $changeLogDbName.$tableName($primaryColumnNames)
           |VALUES(${primaryColumnNameList.map("OLD." + _).mkString(", ")})"""
      ).map(_.stripMargin.replace('\n', ' '))

      val queryForChangeLog =
        s"""CREATE TABLE IF NOT EXISTS $changeLogDbName.$tableName
           |(updated_at TIMESTAMP not null, PRIMARY KEY($primaryColumnNames))
           |AS SELECT $primaryColumnNames FROM $observeeDbName.$tableName
           |WHERE FALSE""".stripMargin.replace('\n', ' ')

      (changeLogger, queryForChangeLog)
    }

    val temp = queries.unzip
    val queriesForObservee = temp._1.flatten
    val queriesForChangeLog = temp._2

    SetupQueries(queriesForObservee, queriesForChangeLog)
  }

  def setUp() = {
    using(DB(connectionPool.borrow)) { db =>
      db.conn.setCatalog(observeeDbName)
      db.autoCommit { implicit session =>
        setUpQueries.forObservee.foreach(SQL(_).execute.apply())
      }
    }

    using(DB(connectionPool.borrow)) { db =>
      db.conn.setCatalog(changeLogDbName)
      db.autoCommit { implicit session =>
        setUpQueries.forChangeLog.foreach(SQL(_).execute.apply())
      }
    }

  }

  lazy val tearDownQueries = using(DB(connectionPool.borrow)) { db =>
    db.readOnly { implicit session =>
      val q = "SELECT TRIGGER_NAME FROM information_schema.triggers " +
        s"WHERE TRIGGER_SCHEMA = '$observeeDbName'"
      val triggerNames = SQL(q).map(_.string(1)).list.apply()
      for (triggerName <- triggerNames)
        yield s"DROP TRIGGER $observeeDbName.$triggerName"
    }
  }

  def tearDown() = using(DB(connectionPool.borrow)) { db =>
    db.conn.setCatalog(observeeDbName)
    db.autoCommit { implicit session =>
      tearDownQueries.foreach(SQL(_).execute.apply())
    }
  }

}
