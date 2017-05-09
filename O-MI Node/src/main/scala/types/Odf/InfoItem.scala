package types
package odf

import scala.collection.{ Seq, Map }
import scala.collection.immutable.HashMap 

import parsing.xmlGen.scalaxb.DataRecord
import parsing.xmlGen.xmlTypes.{InfoItemType}

class InfoItem( 
  val nameAttribute: String,
  val path: Path,
  val name: Seq[QlmID] = Vector.empty,
  val description: Seq[Description]= Vector.empty,
  val value: Seq[Value[Any]]= Vector.empty,
  val metaData: Option[MetaData] = None,
  val attributes: Map[String,String] = HashMap.empty
) extends Node with Unionable[InfoItem]{

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
    name: Seq[QlmID] = this.name,
    description: Seq[Description]= this.description,
    value: Seq[Value[Any]]= this.value,
    metaData: Option[MetaData] = this.metaData,
    attributes: Map[String,String] = this.attributes
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

  implicit def asInfoItemType: InfoItemType = {
    InfoItemType(
      description = description.map( des => des.asDescriptionType ).toSeq,
      MetaData = metaData.map(_.asMetaDataType).toSeq,
      iname = name.map{
        qlmid => qlmid.asQlmIDType
      },
      //Seq(QlmIDType(path.lastOption.getOrElse(throw new IllegalArgumentException(s"OdfObject should have longer than one segment path: $path")))),
      value = value.map{ 
        value : Value[Any] => value.asValueType
      }.toSeq,
      attributes = HashMap{
        "@name" -> DataRecord(
          nameAttribute
        )
      } ++ attributesToDataRecord( attributes )
    )
  }

  implicit def asOdfInfoItem: types.OdfTypes.OdfInfoItem ={
    types.OdfTypes.OdfInfoItem(
      types.Path( path.toSeq ),
      value.map( _.asOdfValue ).toVector,
      description.map( _.asOdfDescription ).headOption,
      metaData.map( _.asOdfMetaData )
    )
  }
}
