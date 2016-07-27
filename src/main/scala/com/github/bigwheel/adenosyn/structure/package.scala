package com.github.bigwheel.adenosyn

import argonaut.Argonaut._
import argonaut._
import scalaz.Scalaz._
import scalikejdbc.DBSession
import scalikejdbc.SQL

// あとでstructureとかそのままのjsonじゃなくてjsonの構造を定義するものだという意味を名前にきちんと込める
// TableとかJoinDefinitionも追加するならDSLとか包括的な意味にしたらどうだろう
package object structure {

  sealed trait JsValue {
    def getTableTree: Seq[JoinDefinitionBase]

    /**
      * 使用するカラム一覧を出す。ただしJoinDefinitionで使われるカラムは除く
      * (そちらは統合したツリーに対して計算する方が楽であるため、別で計算する)
      */
    def listUseColumns: Seq[(TableName, ColumnName, ScalaTypeName)]
  }

  final case class JsObject(
    jdo: Option[JoinDefinitionBase],
    properties: Map[String, JsValue]) extends JsValue {
    def getTableTree: Seq[JoinDefinitionBase] = jdo match {
      case Some(jd) =>
        val newJds = properties.values.flatMap(_.getTableTree).toSeq ++ jd.childSide.joinDefinitions
        Seq(jd.copy(jd.childSide.update(newJds)))
      case None =>
        properties.values.flatMap(_.getTableTree).toSeq
    }

    def listUseColumns = properties.values.flatMap(_.listUseColumns).toSeq
  }

  final case class JsArray(jdo: Option[JoinDefinitionBase], elem: JsValue) extends JsValue {
    def getTableTree: Seq[JoinDefinitionBase] = jdo match {
      case Some(jd) =>
        val newJds = elem.getTableTree ++ jd.childSide.joinDefinitions
        Seq(jd.copy(new Table(jd.childSide.name, newJds: _*)))
      case None =>
        elem.getTableTree
    }

    def listUseColumns = elem.listUseColumns
  }

  final case class JsString(tableName: String, columnName: String) extends JsValue {
    def getTableTree: Seq[JoinDefinitionBase] = Seq.empty

    def listUseColumns = Seq((tableName, columnName, "String"))
  }

  final case class JsInt(tableName: String, columnName: String) extends JsValue {
    def getTableTree: Seq[JoinDefinitionBase] = Seq.empty

    def listUseColumns = Seq((tableName, columnName, "Int"))
  }

  type TableName = String
  type ColumnName = String
  type ScalaTypeName = String

  case class Table(name: TableName, joinDefinitions: JoinDefinitionBase*) {
    def update(_joinDefinitions: Seq[JoinDefinitionBase]) = new Table(name, _joinDefinitions: _*)

    def digUpTables: Seq[Table] = this +: joinDefinitions.flatMap(_.digUpTables)

    def listUseColumns: Seq[(TableName, ColumnName, ScalaTypeName)] =
      joinDefinitions.flatMap(_.listUseColumns(this.name))

    def appendColumns(columnDetails: Map[TableName, Map[ColumnName, ScalaTypeName]]): TableForConstruct =
      new TableForConstruct(name,
        joinDefinitions.map(_.appendColumns(columnDetails)),
        columnDetails(name))
  }

  class TableWithColumns(table: Table, columns: Seq[(ColumnName, ScalaTypeName)]) extends
    Table(table.name, table.joinDefinitions: _*)

  sealed trait JoinDefinitionBase {
    val childSide: Table

    def copy(childSide: Table): JoinDefinitionBase

    def digUpTables: Seq[Table] = childSide.digUpTables

    // これ以下のやつはJoinDefinitionクラスでのみ使われるので、構造を見なおしたらここで定義する必要なくなる
    def listUseColumns(parentTableName: TableName): Seq[(TableName, ColumnName, ScalaTypeName)]

    def appendColumns(columnDetails: Map[TableName, Map[ColumnName, ScalaTypeName]]): JoinDefinitionForConstruct
  }

  case class RootJoinDefinition(childSide: Table) extends JoinDefinitionBase {
    override def copy(childSide: Table): JoinDefinitionBase = new RootJoinDefinition(childSide)

    override def listUseColumns(parentTableName: TableName) = childSide.listUseColumns

    override def appendColumns(columnDetails: Map[TableName, Map[ColumnName, ScalaTypeName]]) = {
      throw new IllegalStateException("ここにJoinDefinition派生クラス以外がくるはずはない")
    }
  }

  case class JoinDefinition(
    parentSideColumn: (ColumnName, ScalaTypeName),
    groupBy: Boolean,
    childSideColumn: (ColumnName, ScalaTypeName),
    childSide: Table
  ) extends JoinDefinitionBase {
    override def copy(childSide: Table): JoinDefinitionBase =
      new JoinDefinition(this.parentSideColumn, this.groupBy, this.childSideColumn, childSide)

    override def listUseColumns(parentTableName: TableName) = {
      Seq(
        (parentTableName, parentSideColumn._1, parentSideColumn._2),
        (childSide.name, childSideColumn._1, childSideColumn._2)
      ) ++ childSide.listUseColumns
    }

    override def appendColumns(columnDetails: Map[TableName, Map[ColumnName, ScalaTypeName]]) =
      new JoinDefinitionForConstruct(parentSideColumn, groupBy, childSideColumn,
        childSide.appendColumns(columnDetails))
  }

  //------------------------------------//
  // ここから旧tableパッケージ内のコード
  //------------------------------------//

  def fetchJsonResult(jsValue: JsValue)(implicit session: DBSession): List[Json] = {
    val tableTreeWithoutColumns = digAndMergeTableTree(jsValue)
    val columnDetails = enumerateUseColumnsByTable(jsValue, tableTreeWithoutColumns)
    val tableTree = appendColumnInfoToTree(tableTreeWithoutColumns, columnDetails)

    val queryString: SqlQuery = tableTree.toSql
    val sqlResult: List[Map[String, Any]] = SQL(queryString).map(_.toMap).list.apply()
    val parsedColumnss: List[List[ParsedColumn]] = structureSqlResult(sqlResult)
    def sqlResultToJson: List[Json] = toJsonObj(parsedColumnss, jsValue)
    sqlResultToJson
  }

  private def digAndMergeTableTree(jsValue: JsValue): Table = jsValue.getTableTree.head.childSide

  private def enumerateUseColumnsByTable(jsValue: JsValue,
    tableTree: Table): Map[TableName, Map[ColumnName, ScalaTypeName]] = {
    val columns = jsValue.listUseColumns ++ tableTree.listUseColumns
    columns.groupBy(_._1).mapValues(_.map { c => (c._2, c._3) }.toMap)
  }

  private type SqlQuery = String

  private def appendColumnInfoToTree(tableTree: Table,
    columnDetails: Map[TableName, Map[ColumnName, ScalaTypeName]]): TableForConstruct =
    tableTree.appendColumns(columnDetails)

  class TableForConstruct(
    val name: TableName,
    joinDefinitions: Seq[JoinDefinitionForConstruct],
    columnNameAndTypeMap: Map[ColumnName, ScalaTypeName]) {

    private[this] def fullColumnInfos = columnNameAndTypeMap.map { case (columnName, scalaTypeName) =>
      new FullColumnInfo(this, columnName, scalaTypeName)
    }.toSet

    def toSql: SqlQuery = toSql(0)._1

    private[structure] def toSql(nestLevel: Int): (SqlQuery, Set[FullColumnInfo]) = {
      val children = joinDefinitions.zipWithIndex.map { case (jd, i) => jd.toSql(this,
        s"_$i",
        nestLevel)
      }
      val allFcis = fullColumnInfos ++ children.flatMap(_._2)
      val sql = s"SELECT ${allFcis.getSelectSqlBody} FROM $name"

      ((sql +: children.map(_._1)).mkString(" "), allFcis)
    }
  }

  class JoinDefinitionForConstruct(
    parentSideColumn: (ColumnName, ScalaTypeName),
    groupBy: Boolean,
    childSideColumn: (ColumnName, ScalaTypeName),
    childSide: TableForConstruct
  ) {

    private[this] def groupedBy(childTable: TableForConstruct,
      newTableName: String) = if (groupBy) {
      val newFCI = new FullColumnInfo(childTable, childSideColumn._1, childSideColumn._2).
        updateTableName(newTableName)
      s" GROUP BY ${newFCI.nowColumnName}"
    } else
      ""

    private[this] def onPart(parentTable: TableForConstruct,
      childTable: TableForConstruct,
      newChildTableName: TableName) = {
      val newChildColumnName = new FullColumnInfo(childTable, childSideColumn._1,
        childSideColumn._2).nowColumnName
      s"ON ${new FQCN(parentTable, parentSideColumn._1).toSql} = ${
        new FQCN(newChildTableName,
          newChildColumnName).toSql
      }"
    }

    private[structure] def toSql(parentTable: TableForConstruct,
      newChildTableName: String,
      nestLevel: Int):
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
      val shallowNestSql = s"SELECT ${newFcis.getSelectSqlBody} FROM ( $sql ) AS A ${
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

  // Fully Qualified Column Name テーブル名も省略していないカラム名(勝手に命名)
  case class FQCN(val tableName: TableName, columnName: String) {
    def this(table: TableForConstruct, columnName: ColumnName) = this(table.name, columnName)

    val toSql = tableName + "." + columnName
  }

  object FullColumnInfo {

    implicit class RichFullColumnInfoSet(fciSet: Set[FullColumnInfo]) {
      def getSelectSqlBody: String = fciSet.map(_.toColumnDefinition).mkString(", ")
    }

  }

  class FullColumnInfo(val columnExpression: String,
    val nowColumnName: String,
    val originalColumn: FQCN) {
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

    def updateTableName(newTableName: TableName): FullColumnInfo =
      new FullColumnInfo(new FQCN(newTableName, this.nowColumnName),
        this.nowColumnName,
        this.originalColumn)
  }

  // TODO: 現在のプログラム上の制約。同じテーブルを複数回JOINすることができない(上で肉付けするときに
  // どちらのテーブルかわからないから

  private case class ParsedColumn(tableName: String,
    columnName: String,
    dimention: Int,
    value: Any) {
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
        val result =
          """\A(.+)__(.+?)__(.+?)(s*)\Z""".r("tableName", "columnName", "type", "dimention").
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
        ParsedColumn(result.group("tableName"),
          result.group("columnName"),
          dim,
          drill(0, value.toString))
      }
    }
    a.map(_.toList)
  }

  private def toJsonObj(parsedColumnss: List[List[ParsedColumn]],
    jsonStructure: JsValue): List[Json] = {
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
