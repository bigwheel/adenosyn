package com.github.bigwheel.adenosyn.changeloggermanager

import scalikejdbc._

class QueryCreator(observeeDbName: String,
  changeLogDbName: String, connectionPool: ConnectionPool) {

  private[this] def tableDetails = using(DB(connectionPool.borrow)) { db =>
    db.autoClose(false)
    // http://stackoverflow.com/a/13433382/4006322
    db.conn.setCatalog(observeeDbName)
    for (tableName <- db.getTableNames())
      yield (
        tableName,
        db.getTable(tableName).get.columns.filter(_.isPrimaryKey)
      )
  }.map { case (tableName, primaryColumns) =>
    (
      tableName,
      primaryColumns.map(_.name),
      primaryColumns.map(_.name).mkString(", ")
    )
  }

  def forChangeLoggers = tableDetails.flatMap {
    case (tableName, primaryColumnNames, joinedColumnNames) =>
      def queryHeader(mode: String) =
        s"""CREATE TRIGGER changelogger_${tableName}_${mode.toLowerCase}
           |AFTER ${mode.toUpperCase}
           |ON $observeeDbName.$tableName FOR EACH ROW
           |REPLACE INTO $changeLogDbName.$tableName($joinedColumnNames)""".
          stripMargin.replace('\n', ' ')

      // 'OLD' and 'NEW' are special keyword of trigger
      // https://dev.mysql.com/doc/refman/5.6/ja/trigger-syntax.html
      Seq(
        s"""${queryHeader("insert")}
           |VALUES
           |(${primaryColumnNames.map("NEW." + _).mkString(", ")})""",
        s"""${queryHeader("update")}
           |VALUES
           |(${primaryColumnNames.map("OLD." + _).mkString(", ")}),
           |(${primaryColumnNames.map("NEW." + _).mkString(", ")})""",
        s"""${queryHeader("delete")}
           |VALUES
           |(${primaryColumnNames.map("OLD." + _).mkString(", ")})"""
      ).map(_.stripMargin.replace('\n', ' '))
  }

  def forChangeLogTables = tableDetails.map {
    case (tableName, _, joinedColumnNames) =>
      s"""CREATE TABLE IF NOT EXISTS $changeLogDbName.$tableName
         |(updated_at TIMESTAMP not null, PRIMARY KEY($joinedColumnNames))
         |AS SELECT $joinedColumnNames FROM $observeeDbName.$tableName
         |WHERE FALSE""".stripMargin.replace('\n', ' ')
  }

  def tearDownQueries = using(DB(connectionPool.borrow)) { db =>
    db.readOnly { implicit session =>
      val q = "SELECT TRIGGER_NAME FROM information_schema.triggers " +
        s"WHERE TRIGGER_SCHEMA = '$observeeDbName'"
      val triggerNames = SQL(q).map(_.string(1)).list.apply()
      for (triggerName <- triggerNames)
        yield s"DROP TRIGGER $observeeDbName.$triggerName"
    }
  }
}
