package com.github.bigwheel.adenosyn

import argonaut.Argonaut._
import argonaut._
import scalaz.Scalaz._
import scalikejdbc.DBSession
import scalikejdbc.SQL

package object dsl {

  //--------------------------------------------------------------------------//
  // DSL
  //--------------------------------------------------------------------------//
  type TableName = String
  type ColumnName = String
  type ScalaTypeName = String
  private type SqlQuery = String

  private def toFullColumnPath(tableName: TableName, columnName: ColumnName) =
    tableName + "." + columnName

  case class Column(
    name: ColumnName,
    scalaTypeName: ScalaTypeName,
    isPrimaryKey: Boolean = false
  )

  // Fully Qualified Column Name テーブル名も省略していないカラム名(勝手に命名)
  // TODO: 第二引数をColumnにする
  private[dsl] case class TableNameAndColumn(val tableName: TableName, column: Column) {
    def this(table: TableWithColumns, column: Column) = this(table.name, column)

    val toSql = toFullColumnPath(tableName, column.name)
  }

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
    * SELECT文内でのカラムの式。
    * "selectExpr AS aliasName" の情報と更にその大元のカラムの情報
    * https://dev.mysql.com/doc/refman/5.6/ja/select.html
    */
  private[dsl] class ColumnSelectExpr private(selectExpr: String,
    val aliasName: String,
    val originalColumn: TableNameAndColumn) {

    val toSql = s"$selectExpr AS $aliasName"

    /**
      * †と対応
      */
    def bindUp(nestLevel: Int): ColumnSelectExpr = new ColumnSelectExpr(
      s"GROUP_CONCAT(${this.selectExpr} SEPARATOR ',${nestLevel + 1}')",
      this.aliasName + "s",
      this.originalColumn
    )

    def updateTableName(newTableName: TableName): ColumnSelectExpr = new ColumnSelectExpr(
      toFullColumnPath(newTableName, this.aliasName), this.aliasName, this.originalColumn
    )
  }

  sealed trait JsValue {
    /**
      * 構造定義オブジェクトの中からテーブル構造のみを取り出す(もちろんTreeになる)
      */
    def constructTableTree: Seq[JoinConditionBase]

    /**
      * 使用するカラム一覧を出す。ただしJoinConditionで使われるカラムは除く
      * (そちらは統合したツリーに対して計算する方が楽であるため、別で計算する)
      */
    def listUseColumns: Seq[(TableName, Column)]

    private[this] def toSql: SqlQuery = {
      val tableTree = constructTableTree.head.asInstanceOf[RootJoinCondition].childTable
      // 一見listUserColumnsは統合できそうだが、JoinConditionで使っているカラムが上のテーブルへ依存
      // しているため、tableTreeを作ってからlistしたほうがいい
      val allColumns = this.listUseColumns ++ tableTree.listUseColumns
      val columnsByTableName = allColumns.groupBy(_._1).mapValues(_.map(_._2).toSet)
      val tableTreeWithColumns = tableTree.mergeColumns(columnsByTableName)

      tableTreeWithColumns.toSql
    }

    def fetchJsons(implicit session: DBSession): List[Json] = {
      val rows = SQL(toSql).map(_.toMap).list.apply()
      constructArgonautJson(structureCellss(rows), this)
    }

  }

  final case class JsString(
    tableName: String,
    columnName: String,
    isPrimaryKey: Boolean = false
  ) extends JsValue {
    def constructTableTree = Seq.empty

    def listUseColumns = Seq((tableName, new Column(columnName, "String", isPrimaryKey)))
  }

  final case class JsInt(
    tableName: String,
    columnName: String,
    isPrimaryKey: Boolean = false
  ) extends JsValue {
    def constructTableTree = Seq.empty

    def listUseColumns = Seq((tableName, new Column(columnName, "Int", isPrimaryKey)))
  }

  final case class JsObject(
    jdo: Option[JoinConditionBase],
    properties: Map[String, JsValue]) extends JsValue {
    def constructTableTree =
      jdo.foldLeft(properties.values.toSeq.flatMap(_.constructTableTree)) { (descendantJds, jd) =>
        jd.addChildJoinConditions(descendantJds) +: Nil
      }

    def listUseColumns = properties.values.toSeq.flatMap(_.listUseColumns)
  }

  final case class JsArray(jdo: Option[JoinConditionBase], elem: JsValue) extends JsValue {
    def constructTableTree =
      jdo.foldLeft(elem.constructTableTree) { (descendantJds, jd) =>
        jd.addChildJoinConditions(descendantJds) +: Nil
      }

    def listUseColumns = elem.listUseColumns
  }

  /**
    * Dot
    */
  case class Table(name: TableName, joinConditions: JoinConditionBase*) {
    def addJoinConditions(_joinConditions: Seq[JoinConditionBase]) =
      new Table(name, joinConditions ++ _joinConditions: _*)

    def digUpTables: Seq[Table] = this +: joinConditions.flatMap(_.digUpTables)

    def listUseColumns: Seq[(TableName, Column)] =
      joinConditions.flatMap(_.listUseColumns(this.name))

    private[dsl] def mergeColumns(columnsByTableName: Map[TableName, Set[Column]]) =
      new TableWithColumns(this, columnsByTableName)
  }

  /**
    * Stem
    */
  sealed trait JoinConditionBase {
    protected[this] val childTable: Table

    protected[this] def copy(childTable: Table): JoinConditionBase

    def addChildJoinConditions(childJds: Seq[JoinConditionBase]): JoinConditionBase =
      copy(new Table(childTable.name, childTable.joinConditions ++ childJds: _*))

    def digUpTables: Seq[Table] = childTable.digUpTables

    // これ以下のやつはJoinConditionクラスでのみ使われるので、構造を見なおしたらここで定義する必要なくなる
    def listUseColumns(parentTableName: TableName): Seq[(TableName, Column)]

    private[dsl] def mergeColumns(
      columnsByTableName: Map[TableName, Set[Column]]
    ): JoinConditionWithColumns
  }

  case class RootJoinCondition(childTable: Table) extends JoinConditionBase {
    override protected[this] def copy(childTable: Table) = new RootJoinCondition(childTable)

    override def listUseColumns(_placeHolder: TableName) = childTable.listUseColumns

    override def mergeColumns(columnsByTableName: Map[TableName, Set[Column]]) = {
      throw new IllegalStateException("ここにJoinCondition派生クラス以外がくるはずはない")
    }
  }

  object JoinCondition {
    /**
      * DSLとして書きやすくするため追加
      */
    def apply(
      parentSideColumn: (ColumnName, ScalaTypeName),
      groupBy: Boolean,
      childSideColumn: (ColumnName, ScalaTypeName),
      childTable: Table
    ): JoinCondition = apply(new Column(parentSideColumn._1, parentSideColumn._2), groupBy,
      new Column(childSideColumn._1, childSideColumn._2), childTable)

    def apply(
      parentSideColumn: (ColumnName, ScalaTypeName, Boolean),
      groupBy: Boolean,
      childSideColumn: (ColumnName, ScalaTypeName),
      childTable: Table
    ): JoinCondition = apply(
      new Column(parentSideColumn._1, parentSideColumn._2, parentSideColumn._3), groupBy,
      new Column(childSideColumn._1, childSideColumn._2), childTable)
  }

  /**
    * https://dev.mysql.com/doc/refman/5.6/ja/join.html
    */
  case class JoinCondition(
    parentSideColumn: Column,
    groupBy: Boolean,
    childSideColumn: Column,
    childTable: Table
  ) extends JoinConditionBase {
    override protected[this] def copy(childSide: Table) =
      new JoinCondition(this.parentSideColumn, this.groupBy, this.childSideColumn, childSide)

    override def listUseColumns(parentTableName: TableName) = {
      Seq(
        (parentTableName, parentSideColumn),
        (childTable.name, childSideColumn)
      ) ++ childTable.listUseColumns
    }

    override private[dsl] def mergeColumns(columnsByTableName: Map[TableName, Set[Column]]) =
      new JoinConditionWithColumns(this, columnsByTableName)
  }

  //--------------------------------------------------------------------------//
  // toSqlのためのプライベートクラス
  //--------------------------------------------------------------------------//

  private[dsl] class TableWithColumns private(
    val name: TableName,
    joinConditions: Seq[JoinConditionWithColumns],
    columns: Set[Column]) {

    def this(table: Table, columnsByTableName: Map[TableName, Set[Column]]) = this(
      table.name,
      table.joinConditions.map(_.mergeColumns(columnsByTableName)),
      columnsByTableName(table.name)
    )

    def toSql: SqlQuery = toSql(0)._1

    /**
      * nestLevelはセパレータのレベルを決めるための変数
      */
    private[dsl] def toSql(nestLevel: Int): (SqlQuery, Set[ColumnSelectExpr]) = {
      val children = joinConditions.zipWithIndex.map { case (jd, i) =>
        jd.toSql(this, s"_$i", nestLevel)
      }
      val columnSelectExprs = columns.map(ColumnSelectExpr.ensureColumnInfoToAlias(this, _))
      val allCSEs = columnSelectExprs ++ children.flatMap(_._2)
      val sql = s"SELECT ${allCSEs.getQuery} FROM $name"

      ((sql +: children.map(_._1)).mkString(" "), allCSEs)
    }
  }

  /**
    * データ構造と値はJoinConditionと全く一緒だがTableTree関連のメソッドを分離して
    * わかりやすいようにしている。またこうすることでJoinDefinitonBase関連の多態性も考慮する必要がなくなる
    */
  private[dsl] class JoinConditionWithColumns private(
    parentSideColumn: Column,
    groupBy: Boolean,
    childSideColumn: Column,
    childTable: TableWithColumns
  ) {
    def this(jd: JoinCondition, columnsByTableName: Map[TableName, Set[Column]]) = this(
      jd.parentSideColumn,
      jd.groupBy,
      jd.childSideColumn,
      jd.childTable.mergeColumns(columnsByTableName)
    )

    /**
      * parentTable JoinConditionは親テーブルの情報を持たないので引数により貰う必要がある
      * nestLevel セパレータの番号を決める
      */
    private[dsl] def toSql(parentTable: TableWithColumns,
      newChildTableName: String,
      nestLevel: Int):
    (SqlQuery, Set[ColumnSelectExpr]) = {

      val (nestedSql, nestedCses) = {
        val tempTableName = "A"

        val (sql, oldCses) = childTable.toSql(nestLevel + (if (groupBy) 1 else 0))
        val cses = oldCses.map(_.updateTableName(tempTableName))
        val newCses = cses.map { cse =>
          if (groupBy && cse.originalColumn != new TableNameAndColumn(childTable, childSideColumn))
            cse.bindUp(nestLevel)
          else
            cse
        }
        val groupByStatement = if (groupBy) {
          val childSideColumnSelectExpr = ColumnSelectExpr.
            ensureColumnInfoToAlias(childTable, childSideColumn).updateTableName(tempTableName)
          s" GROUP BY ${childSideColumnSelectExpr.aliasName}"
        } else
          ""

        (s"SELECT ${newCses.getQuery} FROM ( $sql ) AS A $groupByStatement", newCses)
      }

      val finalCses = nestedCses.map(_.updateTableName(newChildTableName))
      val exactlyJoinCondtion = {
        val newChildSideColumnName =
          ColumnSelectExpr.ensureColumnInfoToAlias(childTable, childSideColumn).aliasName
        val parentSideExpr = new TableNameAndColumn(parentTable, parentSideColumn).toSql
        val childSideExpr = toFullColumnPath(newChildTableName, newChildSideColumnName)

        s"ON $parentSideExpr = $childSideExpr"
      }

      (s"JOIN ( $nestedSql ) AS $newChildTableName $exactlyJoinCondtion", finalCses)
    }
  }

  // TODO: 現在のプログラム上の制約。同じテーブルを複数回JOINすることができない(上で肉付けするときに
  // どちらのテーブルかわからないから

  //--------------------------------------------------------------------------//
  // クエリ結果のparse, 構造化のためのクラス
  //--------------------------------------------------------------------------//

  private def structureCellss(rows: List[Map[String, Any]]): List[List[Cell]] =
    for (row <- rows) yield
      for ((engravedColumnName, rawValue) <- row.toList) yield {
        val pattern = """\A(.+)__(.+?)__(.+?)(s*)\Z""".r
        engravedColumnName match {
          case pattern(tableName, trueColumnName, scalaTypeName, dimentionMarker) =>
            val dimention = dimentionMarker.count(_ == 's')

            def drillDown(level: Int, str: String): Any = if (dimention - level == 0) {
              scalaTypeName match {
                case "Int" => str.toInt
                case "String" => str
              }
            } else {
              val splittedValue = str.split("," + (level + 1).toString) // †と対応
              splittedValue.map(drillDown(level + 1, _))
            }

            new Cell(tableName, trueColumnName, dimention, drillDown(0, rawValue.toString))
        }
      }

  /**
    * ExcelのCellのイメージ
    * https://dev.mysql.com/doc/refman/5.6/ja/insert.html
    * などを見るに"Value"と呼ぶべきかもしれない
    *
    * valueはScalaのPrimitive型ないしそれのX次元Array
    */
  private class Cell(val tableName: String,
    val columnName: String,
    dimention: Int,
    val value: Any) {
    // arrayじゃなくても結果返すので注意(ネスト(jsObjecなど)の中から外側の値を参照できるようにするため)
    def drillDown(index: Int): Cell = value match {
      case v: Array[_] => new Cell(tableName, columnName, dimention - 1, v(index))
      case v => new Cell(tableName, columnName, dimention, v)
    }

    val length: Option[Int] = value match {
      case a: Array[_] => a.length.some
      case _ => none
    }
  }

  private def constructArgonautJson(rows: List[List[Cell]], jsonStructure: JsValue): List[Json] = {
    def parseRow(row: List[Cell], jsonStructureTree: JsValue): Json = {
      def getCell(tableName: String, columnName: String): Cell =
        row.find(cell => cell.tableName == tableName && cell.columnName == columnName).get

      jsonStructureTree match {
        case JsObject(_, properties) =>
          Json(properties.map { case (k, v) => k := parseRow(row, v) }.toSeq: _*)
        case JsArray(_, jsValue) =>
          val arrayLengths = row.flatMap(_.length).distinct
          require(arrayLengths.length == 1) // Arrayの長さに矛盾がないか確認
          (0 until arrayLengths.head).map { index =>
            parseRow(row.map(_.drillDown(index)), jsValue)
          }.toList |> jArray.apply
        case JsString(tableName, columnName) =>
          jString(getCell(tableName, columnName).value.asInstanceOf[String])
        case JsInt(tableName, columnName) =>
          jNumber(getCell(tableName, columnName).value.asInstanceOf[Int])
      }
    }

    for (row <- rows) yield parseRow(row, jsonStructure)
  }

}
