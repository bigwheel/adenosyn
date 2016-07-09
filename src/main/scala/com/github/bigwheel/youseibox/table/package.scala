package com.github.bigwheel.youseibox

import argonaut.Argonaut._
import argonaut._
import com.github.bigwheel.youseibox.json._
import scalaz.Scalaz._

/**
  * TODO: tabletreeへのリネームするのはどうだろう
  * table tree
  * 最終的にSQLを作るのが目的
  */
package object table {

  /** @deprecated 作って活用しようとしたが結局冗長にすぎて使っていない
    *            構造をわかりやすく表現するにはいいと思ったんだけど・・・
    */
  case class Dot[D, L](value: D, lines: Line[L, D]*)
  case class Line[L, D](value: L, dot: Dot[D, L])

  type ColumnName = String
  type ScalaTypeName = String
  type SqlQuery = String

  case class Table(name: String, var joinDefinitions: Seq[JoinDefinition]) {
    def this(name: String) = this(name, Seq())
    val columnNameAndTypeMap = collection.mutable.Map[ColumnName, ScalaTypeName]()

    private[this] def fullColumnInfos = columnNameAndTypeMap.map { case (columnName, scalaTypeName) =>
        new FullColumnInfo(this, columnName, scalaTypeName) }.toSet

    def toSql: SqlQuery = toSql(0)._1
    private[table] def toSql(nestLevel: Int): (SqlQuery, Set[FullColumnInfo]) = {
      val children = joinDefinitions.zipWithIndex.map { case (jd, i) => jd.toSql(this, s"_$i", nestLevel) }
      val allFcis = fullColumnInfos ++ children.flatMap(_._2)
      val sql = s"SELECT ${allFcis.getSelectSqlBody} FROM $name"

      ((sql +: children.map(_._1)).mkString(" "), allFcis)
    }

    def digUpTables: Seq[Table] = this +: joinDefinitions.flatMap(_.digUpTables)
  }

  case class JoinDefinition(
    columnOfParentTable: (ColumnName, ScalaTypeName),
    _1toNRelation: Boolean,
    columnOfChildTable: (ColumnName, ScalaTypeName),
    childTable: Table
  ) {
    def groupedBy(childTable: Table, newTableName: String) = if (_1toNRelation) {
      val newFCI = new FullColumnInfo(childTable, columnOfChildTable._1, columnOfChildTable._2).
        updateTableName(newTableName)
      s" GROUP BY ${newFCI.nowColumnName}"
    } else
      ""

    private[this] def onPart(parentTable: Table, childTable: Table, newChildTableName: String) = {
      val newChildColumnName = new FullColumnInfo(childTable, columnOfChildTable._1,
        columnOfChildTable._2).nowColumnName
      s"ON ${new FQCN(parentTable, columnOfParentTable._1).toSql} = ${new FQCN(newChildTableName, newChildColumnName).toSql}"
    }

    private[table] def toSql(parentTable: Table, newChildTableName: String, nestLevel: Int):
    (SqlQuery, Set[FullColumnInfo]) = {
      val plusLevel = if (_1toNRelation) 1 else 0
      val (sql, oldFcis) = childTable.toSql(nestLevel + plusLevel)
      val fcis = oldFcis.map(_.updateTableName("A"))
      val newFcis = if (_1toNRelation) {
        fcis.map { fci =>
          if (fci.originalColumn == new FQCN(childTable, columnOfChildTable._1))
            fci
          else
            fci.bindUp(nestLevel)
        }
      } else fcis
      val newNewFcis = newFcis.map(_.updateTableName(newChildTableName))
      val shallowNestSql = s"SELECT ${newFcis.getSelectSqlBody} FROM ( $sql ) AS A ${groupedBy(childTable, "A")}"
      (s"JOIN ( $shallowNestSql ) AS $newChildTableName ${onPart(parentTable, childTable, newChildTableName)}", newNewFcis)
    }

    def digUpTables: Seq[Table] = childTable.digUpTables
  }

  // Fully Qualified Column Name テーブル名も省略していないカラム名(勝手に命名)
  case class FQCN(val tableName: String, columnName: String) {
    def this(table: Table, columnName: ColumnName) = this(table.name, columnName)
    val toSql = tableName + "." + columnName
  }

  object FullColumnInfo {
    implicit class RichFullColumnInfoSet(fciSet: Set[FullColumnInfo]) {
      def getSelectSqlBody: String = fciSet.map(_.toColumnDefinition).mkString(", ")
    }
  }

  class FullColumnInfo(val columnExpression: String, val nowColumnName: String, val originalColumn: FQCN) {
    def this(column: FQCN, nowColumnName: String, originalColumn: FQCN) = this(
      column.toSql, nowColumnName, originalColumn
    )
    def this(table: Table, columnName: ColumnName, scalaTypeName: ScalaTypeName) = this(
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

  def toTableStructure(jsValue: JsValue): Table = {
    def a(jsValue: JsValue): Seq[JoinDefinition] = jsValue match {
      case JsObject(Some(jd), b) =>
        jd.childTable.joinDefinitions = (b.values.flatMap(a) ++ jd.childTable.joinDefinitions).toSeq
        Seq(jd)
      case JsArray(Some(jd), b) =>
        jd.childTable.joinDefinitions = a(b) ++ jd.childTable.joinDefinitions
        Seq(jd)
      case JsObject(None, b) =>
        b.values.flatMap(a).toSeq
      // TDOO: arrayでtable definitonが空のテストケースを作れ、それでこの下に追加しろ
      case _ =>
        Seq.empty
    }
    val h = a(jsValue).head.childTable
    // こっから肉付けするよ
    // 返り値はそれぞれ tableName, columnName, scalaTypeName を表す
    def b(jsValue: JsValue): Seq[(String, String, ScalaTypeName)] = jsValue match {
      case JsObject(_, p) =>
        p.values.flatMap(b).toSeq
      case JsArray(_, e) =>
        b(e)
      case JsString(tableName, columnName) =>
        Seq((tableName, columnName, "String"))
      case JsInt(tableName, columnName) =>
        Seq((tableName, columnName, "Int"))
    }
    def enumerateColumnsForOnOfJoin(table: Table): Seq[(String, String, ScalaTypeName)] =
      table.joinDefinitions.flatMap { jd =>
        Seq(
          (table.name, jd.columnOfParentTable._1, jd.columnOfParentTable._2),
          (jd.childTable.name, jd.columnOfChildTable._1, jd.columnOfChildTable._2)
        ) ++ enumerateColumnsForOnOfJoin(jd.childTable)
      }

    val columnDetails = b(jsValue) ++ enumerateColumnsForOnOfJoin(h)
    for (table <- h.digUpTables)
      for (columnDetail <- columnDetails)
        if (table.name == columnDetail._1)
          table.columnNameAndTypeMap += columnDetail._2 -> columnDetail._3

    h
  }

  // TODO: 現在のプログラム上の制約。同じテーブルを複数回JOINすることができない(上で肉付けするときに
  // どちらのテーブルかわからないから

  case class ParsedColumn(tableName: String, columnName: String, dimention: Int, value: Any) {
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

  def structureSqlResult(sqlResult: List[Map[String, Any]]): List[List[ParsedColumn]] = {
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

  def toJsonObj(parsedColumnss: List[List[ParsedColumn]] , jsonStructure: JsValue): List[Json] = {
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
