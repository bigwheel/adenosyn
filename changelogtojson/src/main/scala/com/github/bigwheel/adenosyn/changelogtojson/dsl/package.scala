package com.github.bigwheel.adenosyn.changelogtojson

/**
  * Purpose: generate sql for RDBMS to JSON
  */
package object dsl {

  type TableName = String
  type ColumnName = String
  type ScalaTypeName = String

  case class Table(name: TableName, joinDefinitions: JoinDefinitionBase*) {
    private[dsl] def update(_joinDefinitions: Seq[JoinDefinitionBase]) = new Table(name, _joinDefinitions: _*)

    private[dsl] def digUpTables: Seq[Table] = this +: joinDefinitions.flatMap(_.digUpTables)

    private[dsl] def listUseColumns: Seq[(TableName, Column)] =
      joinDefinitions.flatMap(_.listUseColumns(this.name))

    private[dsl] def appendColumns(columnDetails: Map[TableName, Seq[Column]]): TableWithColumns =
      new TableWithColumns(name,
        joinDefinitions.map(_.appendColumns(columnDetails)),
        columnDetails(name))
  }

  case class Column(val name: ColumnName, val scalaTypeName: ScalaTypeName)



  //------------------------------------//
  // .*JoinDefinition.*
  //------------------------------------//

  sealed trait JoinDefinitionBase {
    private[dsl] val childSide: Table

    private[dsl] def digUpTables: Seq[Table] = childSide.digUpTables

    protected[dsl] def copy(childSide: Table): JoinDefinitionBase

    // TODO: refactor class design because following 2 methods be used
    // oly by JoinDefinition not done by RootJoinDefinition
    protected[dsl] def listUseColumns(parentTableName: TableName): Seq[(TableName, Column)]

    protected[dsl] def appendColumns(columnDetails: Map[TableName, Seq[Column]]): JoinDefinitionForConstruct
  }

  final case class RootJoinDefinition(childSide: Table) extends JoinDefinitionBase {
    override protected[dsl] def copy(childSide: Table): JoinDefinitionBase = new RootJoinDefinition(childSide)

    override protected[dsl] def listUseColumns(parentTableName: TableName) = childSide.listUseColumns

    override protected[dsl] def appendColumns(columnDetails: Map[TableName, Seq[Column]]) = {
      throw new IllegalStateException(
        "appendColumns method of RootJoinDefinition could not be called"
      )
    }
  }

  object JoinDefinition {
    def apply(
      parentSideColumn: (ColumnName, ScalaTypeName),
      groupBy: Boolean,
      childSideColumn: (ColumnName, ScalaTypeName),
      childSide: Table
    ): JoinDefinition = apply(new Column(parentSideColumn._1, parentSideColumn._2), groupBy,
      new Column(childSideColumn._1, childSideColumn._2), childSide)
  }

  final case class JoinDefinition(
    parentSideColumn: Column,
    groupBy: Boolean,
    childSideColumn: Column,
    childSide: Table
  ) extends JoinDefinitionBase {
    override protected[dsl] def copy(childSide: Table): JoinDefinitionBase =
      new JoinDefinition(this.parentSideColumn, this.groupBy, this.childSideColumn, childSide)

    override protected[dsl] def listUseColumns(parentTableName: TableName) = {
      Seq(
        (parentTableName, parentSideColumn),
        (childSide.name, childSideColumn)
      ) ++ childSide.listUseColumns
    }

    override protected[dsl] def appendColumns(columnDetails: Map[TableName, Seq[Column]]) =
      new JoinDefinitionForConstruct(parentSideColumn, groupBy, childSideColumn,
        childSide.appendColumns(columnDetails))
  }



  //------------------------------------//
  // Js.*
  //------------------------------------//

  type SqlQuery = String

  sealed trait JsValue {
    /**
      * Extract only table structue from JsValue tree
      */
    protected[dsl] def constructTableTree: Seq[JoinDefinitionBase]

    /**
      * Enumerate used columns except used in JoinDefinitions
      */
    protected[dsl] def listUseColumns: Seq[(TableName, Column)]

    final def toSql: SqlQuery = {
      def enumerateUseColumnsByTable(jsValue: JsValue,
        tableTree: Table): Map[TableName, Seq[Column]] = {
        val columns = jsValue.listUseColumns ++ tableTree.listUseColumns
        columns.groupBy(_._1).mapValues(_.map(_._2))
      }
      def appendColumnInfoToTree(tableTree: Table,
        columnDetails: Map[TableName, Seq[Column]]): TableWithColumns =
        tableTree.appendColumns(columnDetails)


      val tableTreeWithoutColumns = constructTableTree.head.childSide
      val columnDetails = enumerateUseColumnsByTable(this, tableTreeWithoutColumns)
      val tableTree = appendColumnInfoToTree(tableTreeWithoutColumns, columnDetails)

      tableTree.toSql
    }
  }

  final case class JsString(tableName: String, columnName: String) extends JsValue {
    protected[dsl] def constructTableTree: Seq[JoinDefinitionBase] = Seq.empty

    protected[dsl] def listUseColumns = Seq((tableName, new Column(columnName, "String")))
  }

  final case class JsInt(tableName: String, columnName: String) extends JsValue {
    protected[dsl] def constructTableTree: Seq[JoinDefinitionBase] = Seq.empty

    protected[dsl] def listUseColumns = Seq((tableName, new Column(columnName, "Int")))
  }

  final case class JsObject(
    jdo: Option[JoinDefinitionBase],
    properties: Map[String, JsValue]) extends JsValue {
    protected[dsl] def constructTableTree: Seq[JoinDefinitionBase] = jdo match {
      case Some(jd) =>
        val newJds = properties.values.flatMap(_.constructTableTree).toSeq ++ jd.childSide.joinDefinitions
        Seq(jd.copy(jd.childSide.update(newJds)))
      case None =>
        properties.values.flatMap(_.constructTableTree).toSeq
    }

    protected[dsl] def listUseColumns = properties.values.flatMap(_.listUseColumns).toSeq
  }

  final case class JsArray(jdo: Option[JoinDefinitionBase], elem: JsValue) extends JsValue {
    protected[dsl] def constructTableTree: Seq[JoinDefinitionBase] = jdo match {
      case Some(jd) =>
        val newJds = elem.constructTableTree ++ jd.childSide.joinDefinitions
        Seq(jd.copy(new Table(jd.childSide.name, newJds: _*)))
      case None =>
        elem.constructTableTree
    }

    protected[dsl] def listUseColumns = elem.listUseColumns
  }




  //------------------------------------//
  // private
  //------------------------------------//

  private[dsl] case class TableNameAndColumn(val tableName: TableName, column: Column) {
    def this(table: TableWithColumns, column: Column) = this(table.name, column)

    val toSql = toFullColumnPath(tableName, column.name)
  }

  private def toFullColumnPath(tableName: TableName, columnName: ColumnName) =
    tableName + "." + columnName

  private object ColumnSelectExpr {

    implicit class RichColumnSelectQuerySet(csqSet: Set[ColumnSelectExpr]) {
      def getQuery: String = csqSet.map(_.toSql).mkString(", ")
    }

    def ensureColumnInfoToAlias(table: TableWithColumns, column: Column) = {
      val tnac = new TableNameAndColumn(table, column)
      new ColumnSelectExpr(tnac.toSql, s"${table.name}__${column.name}__${column.scalaTypeName}",
        tnac)
    }

  }

  /**
    * https://dev.mysql.com/doc/refman/5.6/ja/select.html
    */
  private[dsl] class ColumnSelectExpr private(selectExpr: String,
    val aliasName: String,
    val originalColumn: TableNameAndColumn) {

    val toSql = s"$selectExpr AS $aliasName"

    def bindUp(nestLevel: Int): ColumnSelectExpr = new ColumnSelectExpr(
      s"GROUP_CONCAT(${this.selectExpr} SEPARATOR ',${nestLevel + 1}')",
      this.aliasName + "s",
      this.originalColumn
    )

    def updateTableName(newTableName: TableName): ColumnSelectExpr = new ColumnSelectExpr(
      toFullColumnPath(newTableName, this.aliasName), this.aliasName, this.originalColumn
    )
  }

  private[dsl] class TableWithColumns(val name: TableName,
    joinDefinitions: Seq[JoinDefinitionForConstruct], columns: Seq[Column]) {

    private[this] def columnSelectExprs =
      columns.map(ColumnSelectExpr.ensureColumnInfoToAlias(this, _)).toSet

    def toSql: SqlQuery = toSql(0)._1

    def toSql(nestLevel: Int): (SqlQuery, Set[ColumnSelectExpr]) = {
      val children = joinDefinitions.zipWithIndex.map { case (jd, i) => jd.toSql(this,
        s"_$i",
        nestLevel)
      }
      val allFcis = columnSelectExprs ++ children.flatMap(_._2)
      val sql = s"SELECT ${allFcis.getQuery} FROM $name"

      ((sql +: children.map(_._1)).mkString(" "), allFcis)
    }
  }

  private[dsl] class JoinDefinitionForConstruct(
    parentSideColumn: Column,
    groupBy: Boolean,
    childSideColumn: Column,
    childSide: TableWithColumns
  ) {

    private[this] def groupedBy(childTable: TableWithColumns, newTableName: String) =
      if (groupBy) {
        val newColumnSelectExpr = ColumnSelectExpr.
          ensureColumnInfoToAlias(childTable, childSideColumn).updateTableName(newTableName)
        s" GROUP BY ${newColumnSelectExpr.aliasName}"
      } else
        ""

    private[this] def onPart(parentTable: TableWithColumns,
      childTable: TableWithColumns,
      newChildTableName: TableName) = {
      val newChildColumnName =
        ColumnSelectExpr.ensureColumnInfoToAlias(childTable, childSideColumn).aliasName
      s"ON ${new TableNameAndColumn(parentTable, parentSideColumn).toSql} = ${
        toFullColumnPath(newChildTableName, newChildColumnName)
      }"
    }

    def toSql(parentTable: TableWithColumns,
      newChildTableName: String,
      nestLevel: Int):
    (SqlQuery, Set[ColumnSelectExpr]) = {
      val plusLevel = if (groupBy) 1 else 0
      val (sql, oldFcis) = childSide.toSql(nestLevel + plusLevel)
      val fcis = oldFcis.map(_.updateTableName("A"))
      val newFcis = if (groupBy) {
        fcis.map { fci =>
          if (fci.originalColumn == new TableNameAndColumn(childSide, childSideColumn))
            fci
          else
            fci.bindUp(nestLevel)
        }
      } else fcis
      val newNewFcis = newFcis.map(_.updateTableName(newChildTableName))
      val shallowNestSql = s"SELECT ${newFcis.getQuery} FROM ( $sql ) AS A ${
        groupedBy(childSide,
          "A")
      }"
      (s"JOIN ( $shallowNestSql ) AS $newChildTableName ${
        onPart(parentTable,
          childSide,
          newChildTableName)
      }", newNewFcis)
    }
  }

}
