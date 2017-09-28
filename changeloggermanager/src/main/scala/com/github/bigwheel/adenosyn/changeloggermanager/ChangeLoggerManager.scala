package com.github.bigwheel.adenosyn.changeloggermanager

import scalikejdbc._

case class SetupQueries(
  forChangeLogTable: List[String],
  forChangeLogger: List[String]
)

class ChangeLoggerManager private(observeeDbName: String,
  changeLogDbName: String, connectionPool: ConnectionPool) {

  def this(jdbcUrl: String, observeeDbName: String, changeLogDbName: String,
    username: String, password: String) = this(observeeDbName, changeLogDbName,
    Commons2ConnectionPoolFactory(jdbcUrl, username, password)
  )

  override def finalize(): Unit = connectionPool.close

  val query = new QueryCreator(observeeDbName, changeLogDbName,
    connectionPool)

  def setUp() = {
    using(DB(connectionPool.borrow)) { db =>
      db.conn.setCatalog(changeLogDbName)
      db.autoCommit { implicit session =>
        query.forChangeLogTables.foreach(SQL(_).execute.apply())
      }
    }

    using(DB(connectionPool.borrow)) { db =>
      db.conn.setCatalog(observeeDbName)
      db.autoCommit { implicit session =>
        query.forChangeLoggers.foreach(SQL(_).execute.apply())
      }
    }

    if (!isValid)
      throw new Exception("table definition is not valid(this must be a bug)")
  }

  def tearDown() = using(DB(connectionPool.borrow)) { db =>
    db.conn.setCatalog(observeeDbName)
    db.autoCommit { implicit session =>
      query.tearDownQueries.foreach(SQL(_).execute.apply())
    }
  }

  def isValid(): Boolean = {
    def getAllTables(dbName: String) = using(DB(connectionPool.borrow)) { db =>
      db.autoClose(false)
      // http://stackoverflow.com/a/13433382/4006322
      db.conn.setCatalog(dbName)

      db.getTableNames().map(db.getTable(_).get)
    }

    try {
      val observeeTables = getAllTables(observeeDbName)
      val changeLogTables = getAllTables(changeLogDbName)
      val observeeTableNames = observeeTables.map(_.name).sorted
      val changeLogTableNames = changeLogTables.map(_.name).sorted

      if (observeeTableNames != changeLogTableNames)
        return false

      //val tableNames = observeeTableNames // == changeLogTableNames
      changeLogTables.foreach { table =>
        if (!table.columns.exists(_.name == "updated_at"))
          return false
      }

      val tablePairs = observeeTables.sortBy(_.name).
        zip(changeLogTables.sortBy(_.name))
      tablePairs.foreach { case (observeeTable, changeLogTable) =>
        if (observeeTable.columns.filter(_.isPrimaryKey).toSet !=
          changeLogTable.columns.filter(_.isPrimaryKey).toSet)
          return false
      }
      true

    } catch {
      case e: java.sql.SQLException =>
        // TODO: comment in and use logger in following lines
        //println(e)
        //println("maybe no user")
        false
    }
  }
}
