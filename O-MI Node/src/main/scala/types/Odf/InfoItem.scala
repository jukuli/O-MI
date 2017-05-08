package odf 

import scala.collection.{ Seq, Map }
import scala.collection.immutable.HashMap 

import parsing.xmlGen.scalaxb.DataRecord
import parsing.xmlGen.xmlTypes.{InfoItemType}

class InfoItem( 
  val nameAttribute: String,
  val parentPath: Option[Path] = None,
  val name: Seq[QlmID] = Vector.empty,
  val description: Seq[Description]= Vector.empty,
  val value: Seq[Value[Any]]= Vector.empty,
  val metaData: Option[MetaData] = None,
  val attributes: Map[String,String] = HashMap.empty
) extends Node with Unionable[InfoItem]{

  def union( that: InfoItem ): InfoItem ={
    val pathsMatches = ( parentPath, that.parentPath ) match {
      case (Some(p), Some(op) ) => p == op
      case (None, Some(op) ) => true
      case (Some(p), None ) => true
      case (None, None) => true 
    }
    assert( nameAttribute == that.nameAttribute && pathsMatches )
    new InfoItem(
      nameAttribute,
      parentPath.orElse(that.parentPath),
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
    parentPath: Option[Path] = this.parentPath,
    name: Seq[QlmID] = this.name,
    description: Seq[Description]= this.description,
    value: Seq[Value[Any]]= this.value,
    metaData: Option[MetaData] = this.metaData,
    attributes: Map[String,String] = this.attributes
  ): InfoItem = new InfoItem(
    nameAttribute,
    parentPath,
    name,
    description,
    value,
    metaData,
    attributes
  )
  def createAncestors: Seq[Node] = {
    parentPath.map{
      case originalPath: Path =>
        originalPath.getAncestors.map{
          case ancestorPath: Path => 
            new Object(
              Vector(
                new QlmID(
                  ancestorPath.last
                )
              ),
              Some(ancestorPath)
            )
        }.toVector
    }.getOrElse(Vector())
  }

  implicit def asInfoItemType: InfoItemType = {
    InfoItemType(
      description = description.map( des => des.asDescription ).toSeq,
      MetaData = metaData.map(_.asMetaData).toSeq,
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
}
