package types
package odf

import scala.collection.{ Seq, Map }
import scala.collection.immutable.{ HashMap, Map =>IMap}

import parsing.xmlGen.scalaxb.DataRecord
import parsing.xmlGen.xmlTypes.InfoItemType

case class InfoItem( 
  val nameAttribute: String,
  val path: Path,
  val name: Vector[QlmID] = Vector.empty,
  val description: Vector[Description]= Vector.empty,
  val value: Vector[Value[Any]]= Vector.empty,
  val metaData: Option[MetaData] = None,
  val attributes: IMap[String,String] = HashMap.empty
) extends Node with Unionable[InfoItem]{
  assert( nameAttribute == path.last )

  def union( that: InfoItem ): InfoItem ={
    val pathsMatches = path == that.path
    assert( nameAttribute == that.nameAttribute && pathsMatches )
    new InfoItem(
      nameAttribute,
      path,
      name ++ that.name,
      description ++ that.description,
      value ++ that.value,
      (metaData, that.metaData) match{
        case (Some( md ), Some( omd )) => Some( md.union(omd) )
        case (md,omd) => md.orElse(omd)
      },
      attributes ++ that.attributes
    )
  }
  def copy(
    nameAttribute: String = this.nameAttribute,
    path: Path = this.path,
    name: Vector[QlmID] = this.name,
    description: Vector[Description]= this.description,
    value: Vector[Value[Any]]= this.value,
    metaData: Option[MetaData] = this.metaData,
    attributes: IMap[String,String] = this.attributes
  ): InfoItem = new InfoItem(
    nameAttribute,
    path,
    name,
    description,
    value,
    metaData,
    attributes
  )
  def createAncestors: Seq[Node] = {
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
      this.description.map{ 
        case des: Description => 
          des.asDescriptionType 
      }.toVector,
      this.metaData.map(_.asMetaDataType).toSeq,
      //Seq(QlmIDType(path.lastOption.getOrElse(throw new IllegalArgumentException(s"OdfObject should have longer than one segment path: $path")))),
      this.value.map{ 
        value : Value[Any] => value.asValueType
      }.toSeq,
      HashMap{
        "@name" -> DataRecord(
          nameAttribute
        )
      } ++ attributesToDataRecord( this.attributes )
    )
  }

}
