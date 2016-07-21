package com.github.bigwheel.youseibox

import com.github.bigwheel.youseibox.table.JoinDefinitionForConstruct
import com.github.bigwheel.youseibox.table.TableForConstruct

// あとでstructureとかそのままのjsonじゃなくてjsonの構造を定義するものだという意味を名前にきちんと込める
// TableとかJoinDefinitionも追加するならDSLとか包括的な意味にしたらどうだろう
package object json {

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
      new TableForConstruct(name, joinDefinitions.map(_.appendColumns(columnDetails)), columnDetails(name))
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

}
