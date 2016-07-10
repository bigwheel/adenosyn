package com.github.bigwheel.youseibox

import argonaut.Argonaut._
import argonaut._
import com.github.bigwheel.youseibox.json._
import scalaz.Scalaz._
import scalikejdbc.DBSession
import scalikejdbc.SQL

/**
  * TODO: tabletreeへのリネームするのはどうだろう
  * table tree
  * 最終的にSQLを作るのが目的
  */
package object table {

  def fetchJsonResult(jsValue: JsValue)(implicit session: DBSession): List[Json] = {
    val tableTree: Table = digAndMergeTableTree(jsValue)
    val columnDetails = enumerateUseColumnsByTable(jsValue, tableTree)
    val newTree = appendColumnInfoToTree(tableTree, columnDetails)

    val queryString: SqlQuery = newTree.toSql
    val sqlResult: List[Map[String, Any]] = SQL(queryString).map(_.toMap).list.apply()
    val parsedColumnss: List[List[ParsedColumn]] = structureSqlResult(sqlResult)
    def sqlResultToJson: List[Json] = toJsonObj(parsedColumnss, jsValue)
    sqlResultToJson
  }

  private def digAndMergeTableTree(jsValue: JsValue): Table = {
    def f(jsValue: JsValue): Seq[JoinDefinitionBase] = jsValue match {
      case JsObject(Some(jd), properties) =>
        val newJds = jd.childSide.joinDefinitions ++ properties.values.flatMap(f)
        Seq(jd.copy(new Table(jd.childSide.name, newJds: _*)))
      case JsArray(Some(jd), elem) =>
        val newJds = jd.childSide.joinDefinitions ++ f(elem)
        Seq(jd.copy(new Table(jd.childSide.name, newJds: _*)))
      case JsObject(None, properties) =>
        properties.values.flatMap(f).toSeq
      // TDOO: arrayでtable definitonが空のテストケースを作れ、それでこの下に追加しろ
      case _ =>
        Seq.empty
    }
    f(jsValue).head.childSide
  }

  private type TableName = String

  private type SqlQuery = String

  private def appendColumnInfoToTree(tableTree: Table,
    columnDetails: Map[TableName, Map[ColumnName, ScalaTypeName]]): TableForConstruct = {
    def f(table: Table): TableForConstruct = {
      val columnsForThisTable = columnDetails(table.name)
      new TableForConstruct(table.name, table.joinDefinitions.map(g), columnsForThisTable)
    }
    def g(jd: JoinDefinitionBase): JoinDefinitionForConstruct = jd match {
      case jd : JoinDefinition =>
        new JoinDefinitionForConstruct(jd.parentSideColumn, jd.groupBy, jd.childSideColumn, f(jd.childSide))
      case _ =>
        throw new IllegalStateException("ここにJoinDefinition派生クラス以外がくるはずはない")
    }
    f(tableTree)
  }

  private class TableForConstruct(
    val name: TableName,
    joinDefinitions: Seq[JoinDefinitionForConstruct],
    columnNameAndTypeMap: Map[ColumnName, ScalaTypeName]) {

    private[this] def fullColumnInfos = columnNameAndTypeMap.map { case (columnName, scalaTypeName) =>
        new FullColumnInfo(this, columnName, scalaTypeName) }.toSet

    def toSql: SqlQuery = toSql(0)._1
    private[table] def toSql(nestLevel: Int): (SqlQuery, Set[FullColumnInfo]) = {
      val children = joinDefinitions.zipWithIndex.map { case (jd, i) => jd.toSql(this, s"_$i", nestLevel) }
      val allFcis = fullColumnInfos ++ children.flatMap(_._2)
      val sql = s"SELECT ${allFcis.getSelectSqlBody} FROM $name"

      ((sql +: children.map(_._1)).mkString(" "), allFcis)
    }
  }

  private class JoinDefinitionForConstruct(
    parentSideColumn: (ColumnName, ScalaTypeName),
    groupBy: Boolean,
    childSideColumn: (ColumnName, ScalaTypeName),
    childSide: TableForConstruct
  ) {

    private[this] def groupedBy(childTable: TableForConstruct, newTableName: String) = if (groupBy) {
      val newFCI = new FullColumnInfo(childTable, childSideColumn._1, childSideColumn._2).
        updateTableName(newTableName)
      s" GROUP BY ${newFCI.nowColumnName}"
    } else
      ""

    private[this] def onPart(parentTable: TableForConstruct, childTable: TableForConstruct, newChildTableName: String) = {
      val newChildColumnName = new FullColumnInfo(childTable, childSideColumn._1,
        childSideColumn._2).nowColumnName
      s"ON ${new FQCN(parentTable, parentSideColumn._1).toSql} = ${new FQCN(newChildTableName, newChildColumnName).toSql}"
    }

    private[table] def toSql(parentTable: TableForConstruct, newChildTableName: String, nestLevel: Int):
    (SqlQuery, Set[FullColumnInfo]) = {
      val plusLevel = if (groupBy) 1 else 0
      val (sql, oldFcis) = childSide.toSql(nestLevel + plusLevel)
      val fcis = oldFcis.map(_.updateTableName("A"))
      val newFcis = if (groupBy) {
        fcis.map { fci =>
          if (fci.originalColumn == new FQCN(childSide, childSideColumn._1))
            fci
          else
            fci.bindUp(nestLevel)
        }
      } else fcis
      val newNewFcis = newFcis.map(_.updateTableName(newChildTableName))
      val shallowNestSql = s"SELECT ${newFcis.getSelectSqlBody} FROM ( $sql ) AS A ${groupedBy(childSide, "A")}"
      (s"JOIN ( $shallowNestSql ) AS $newChildTableName ${onPart(parentTable, childSide, newChildTableName)}", newNewFcis)
    }
  }

  // Fully Qualified Column Name テーブル名も省略していないカラム名(勝手に命名)
  private case class FQCN(val tableName: TableName, columnName: String) {
    def this(table: TableForConstruct, columnName: ColumnName) = this(table.name, columnName)
    val toSql = tableName + "." + columnName
  }

  private object FullColumnInfo {
    implicit class RichFullColumnInfoSet(fciSet: Set[FullColumnInfo]) {
      def getSelectSqlBody: String = fciSet.map(_.toColumnDefinition).mkString(", ")
    }
  }

  private class FullColumnInfo(val columnExpression: String, val nowColumnName: String, val originalColumn: FQCN) {
    def this(column: FQCN, nowColumnName: String, originalColumn: FQCN) = this(
      column.toSql, nowColumnName, originalColumn
    )
    def this(table: TableForConstruct, columnName: ColumnName, scalaTypeName: ScalaTypeName) = this(
      new FQCN(table, columnName), s"${table.name}__${columnName}__$scalaTypeName",
      new FQCN(table, columnName))

    val toColumnDefinition = s"$columnExpression AS $nowColumnName"

    def bindUp(nestLevel: Int): FullColumnInfo = new FullColumnInfo(
      s"GROUP_CONCAT(${this.columnExpression} SEPARATOR ',${nestLevel + 1}')",
      this.nowColumnName + "s",
      this.originalColumn
    )

    def updateTableName(newTableName: String): FullColumnInfo =
      new FullColumnInfo(new FQCN(newTableName, this.nowColumnName), this.nowColumnName, this.originalColumn)
  }

  private def enumerateUseColumnsByTable(jsValue: JsValue,
    tableTree: Table): Map[TableName, Map[ColumnName, ScalaTypeName]] = {
    def enumerateByJsValue(jsValue: JsValue): Seq[(TableName, ColumnName, ScalaTypeName)] = jsValue match {
      case JsObject(_, p) =>
        p.values.flatMap(enumerateByJsValue).toSeq
      case JsArray(_, e) =>
        enumerateByJsValue(e)
      case JsString(tableName, columnName) =>
        Seq((tableName, columnName, "String"))
      case JsInt(tableName, columnName) =>
        Seq((tableName, columnName, "Int"))
    }
    def enumerateColumnsForOnOfJoin(table: Table): Seq[(TableName, ColumnName, ScalaTypeName)] =
      table.joinDefinitions.flatMap {
        case (jd: JoinDefinition) =>
          Seq(
            (table.name, jd.parentSideColumn._1, jd.parentSideColumn._2),
            (jd.childSide.name, jd.childSideColumn._1, jd.childSideColumn._2)
          ) ++ enumerateColumnsForOnOfJoin(jd.childSide)
        case (jd: RootJoinDefinition) =>
          enumerateColumnsForOnOfJoin(jd.childSide)
      }

    val columns = enumerateByJsValue(jsValue) ++ enumerateColumnsForOnOfJoin(tableTree)
    columns.groupBy(_._1).mapValues(_.map { c => (c._2, c._3) }.toMap)
  }

  // TODO: 現在のプログラム上の制約。同じテーブルを複数回JOINすることができない(上で肉付けするときに
  // どちらのテーブルかわからないから

  private case class ParsedColumn(tableName: String, columnName: String, dimention: Int, value: Any) {
    // arrayじゃなくても結果返すので注意
    def drill(index: Int): ParsedColumn = value match {
      case a: Array[_] => new ParsedColumn(tableName, columnName, dimention - 1, a(index))
      case a => new ParsedColumn(tableName, columnName, dimention, a)
    }

    val length: Option[Int] = value match {
      case a: Array[_] => a.length.some
      case _ => none
    }
  }

  private def structureSqlResult(sqlResult: List[Map[String, Any]]): List[List[ParsedColumn]] = {
    val a = for (row <- sqlResult) yield {
      for ((columnName, value) <- row) yield {
        val result = """\A(.+)__(.+?)__(.+?)(s*)\Z""".r("tableName", "columnName", "type", "dimention").
          findFirstMatchIn(columnName).get

        val dim = result.group("dimention").count(_ == 's')
        def drill(level: Int, str: String): Any = if (dim - level == 0) {
          result.group("type") match {
            case "Int" => str.toInt
            case "String" => str
          }
        } else {
          val splitted = str.asInstanceOf[String].split("," + (level + 1).toString)
          splitted.map(drill(level + 1, _))
        }
        ParsedColumn(result.group("tableName"), result.group("columnName"), dim, drill(0, value.toString))
      }
    }
    a.map(_.toList)
  }

  private def toJsonObj(parsedColumnss: List[List[ParsedColumn]] , jsonStructure: JsValue): List[Json] = {
    def f(row: List[ParsedColumn], jsonTree: JsValue): Json = {
      def getColumn(tableName: String, columnName: String): ParsedColumn =
        row.find(c => c.tableName == tableName && c.columnName == columnName).get

      jsonTree match {
        case JsObject(_, properties) =>
          Json(properties.map { case (k, v) => k := f(row, v) }.toSeq: _*)
        case JsArray(_, jsValue) =>
          val arrayLengths = row.flatMap(_.length).distinct
          require(arrayLengths.length == 1)
          (0 until arrayLengths.head).map { index =>
            f(row.map(_.drill(index)), jsValue)
          }.toList |> jArray.apply
        case JsString(tableName, columnName) =>
          jString(getColumn(tableName, columnName).value.asInstanceOf[String])
        case JsInt(tableName, columnName) =>
          jNumber(getColumn(tableName, columnName).value.asInstanceOf[Int])
      }
    }

    for (row <- parsedColumnss) yield f(row, jsonStructure)
  }

}
