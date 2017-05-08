package odf

import scala.collection.{ Seq, Map }
import parsing.xmlGen.xmlTypes.MetaDataType

class MetaData(
  val infoItems: Set[InfoItem]
) extends Unionable[MetaData] {
  lazy val nameToII: Map[String, InfoItem] = infoItems.map{ ii => ii.nameAttribute ->ii }.toMap
  lazy val names: Set[String] = infoItems.map{ ii => ii.nameAttribute }.toSet
  def union( that: MetaData ): MetaData ={

    val intersectingNames = names.intersect( that.names )
    val intersectedII = intersectingNames.flatMap{
      case name: String =>
        ( nameToII.get(name), that.nameToII.get(name) ) match {
          case (Some( ii ), Some( tii) ) => Some( ii.union(tii) )
          case (ii,tii) =>  ii.orElse(tii)
        }
    }
    new MetaData( 
      (names -- intersectingNames).flatMap{
        case name: String => 
          nameToII.get(name)
      } ++
      (that.names -- intersectingNames).flatMap{
        case name: String => 
          that.nameToII.get(name)
      } ++ intersectedII
    )
  }
  implicit def asMetaData : MetaDataType = MetaDataType( infoItems.map(_.asInfoItemType ).toSeq )
}
