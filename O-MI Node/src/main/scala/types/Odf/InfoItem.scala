package types
package odf

import scala.collection.{ Seq, Map }
import scala.collection.immutable.{ HashMap, Map =>IMap}

import parsing.xmlGen.scalaxb.DataRecord
import parsing.xmlGen.xmlTypes.InfoItemType

object InfoItem{
  def apply(path: Path, values: Vector[Value[Any]] ): InfoItem ={
    InfoItem(
      path.last,
      path,
      values = values
    )
  }
  def apply(
    path: Path,
    typeAttribute: Option[String],
    name: Vector[QlmID],
    descriptions: Vector[Description],
    values: Vector[Value[Any]],
    metaData: Option[MetaData],
    attributes: IMap[String,String]
  ): InfoItem ={
    InfoItem(
      path.last,
      path,
      typeAttribute,
      name,
      descriptions,
      values,
      metaData,
      attributes
    )
  }
}

case class InfoItem( 
  val nameAttribute: String,
  val path: Path,
  val typeAttribute: Option[String] = None,
  val name: Vector[QlmID] = Vector.empty,
  val descriptions: Vector[Description]= Vector.empty,
  val values: Vector[Value[Any]]= Vector.empty,
  val metaData: Option[MetaData] = None,
  val attributes: IMap[String,String] = HashMap.empty
) extends Node with Unionable[InfoItem]{
  assert( nameAttribute == path.last )
  def intersection( that: InfoItem ): InfoItem ={
    val typeMatches = typeAttribute.forall{
      case typeStr: String => 
        that.typeAttribute.forall{
          case otherTypeStr: String => typeStr == otherTypeStr
        }
    }
    val pathsMatches = path == that.path
    assert( nameAttribute == that.nameAttribute && pathsMatches && typeMatches )
    new InfoItem(
      nameAttribute,
      path,
      that.typeAttribute.orElse( typeAttribute),
      if( that.name.nonEmpty ){
        QlmID.unionReduce( that.name ++ name).toVector.filter{ case id => id.id.nonEmpty}
      } else Vector.empty,
      if( that.descriptions.nonEmpty ){
        Description.unionReduce(that.descriptions ++ descriptions).toVector.filter{ case desc => desc.text.nonEmpty}
      } else Vector.empty,
      values,
      (metaData, that.metaData) match{
        case (Some( md ), Some( omd )) => Some( omd.union(md) )
        case ( Some(md), None) => Some( MetaData( Vector()))
        case (None, _) => None 
      },
      that.attributes ++ attributes 
    )
  }

  def union( that: InfoItem ): InfoItem ={
    val typeMatches = typeAttribute.forall{
      case typeStr: String => 
        that.typeAttribute.forall{
          case otherTypeStr: String => typeStr == otherTypeStr
        }
    }
    val pathsMatches = path == that.path
    assert( nameAttribute == that.nameAttribute && pathsMatches && typeMatches )
    new InfoItem(
      nameAttribute,
      path,
      typeAttribute,
      QlmID.unionReduce(name ++ that.name).toVector,
      Description.unionReduce(descriptions ++ that.descriptions).toVector,
      values ++ that.values,
      (metaData, that.metaData) match{
        case (Some( md ), Some( omd )) => Some( md.union(omd) )
        case (md,omd) => md.orElse(omd)
      },
      attributes ++ that.attributes
    )
  }
  def createAncestos: Seq[Node] = {
        path.getAncestors.map{
          case ancestorPath: Path => 
            new Object(
              Vector(
                new QlmID(
                  ancestorPath.last
                )
              ),
              ancestorPath
            )
        }.toVector
  }
  def createParent: Node = {
    val parentPath = path.init
    if( parentPath == new Path( "Objects") ){
      new Objects()
    } else {
      new Object(
        Vector(
          new QlmID(
            parentPath.last
          )
        ),
        parentPath
      )
    }
  }

  def asInfoItemType: InfoItemType = {
    InfoItemType(
      this.name.map{
        qlmid => qlmid.asQlmIDType
      },
      this.descriptions.map{ 
        case des: Description => 
          des.asDescriptionType 
      }.toVector,
      this.metaData.map(_.asMetaDataType).toSeq,
      //Seq(QlmIDType(path.lastOption.getOrElse(throw new IllegalArgumentException(s"OdfObject should have longer than one segment path: $path")))),
      this.values.map{ 
        value : Value[Any] => value.asValueType
      }.toSeq,
      HashMap(
        "@name" -> DataRecord(
          nameAttribute
        ),
        "@type" -> DataRecord(
          typeAttribute
        )
      ) ++ attributesToDataRecord( this.attributes )
    )
  }

}
