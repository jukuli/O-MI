package odf

import scala.collection.{ Seq, Map }
import scala.collection.immutable.HashMap

import java.sql.Timestamp
import parsing.xmlGen.xmlTypes.QlmIDType
import parsing.xmlGen.scalaxb.DataRecord

case class QlmID(
  val id: String,
  val idType: Option[String] =  None,
  val tagType: Option[String] =  None,
  val endDate: Option[Timestamp] =  None,
  val startDate: Option[Timestamp] =  None,
  val attributes: Map[String,String] =  HashMap.empty
) {
  
  def asQlmIDType: QlmIDType = {
    val idTypeAttr: Seq[(String,DataRecord[Any])] = idType.map{
          typ =>
            ("@idType" -> DataRecord(typ))
        }.toSeq 
    val tagTypeAttr: Seq[(String,DataRecord[Any])]  = tagType.map{
          typ =>
            ("@tagType" -> DataRecord(typ))
        }.toSeq  
  
    val startDateAttr = startDate.map{
              startDate =>
                ("@startDate" -> DataRecord(timestampToXML(startDate)))
            }.toSeq
    val endDateAttr = endDate.map{
              endDate =>
                ("@endDate" -> DataRecord(timestampToXML(endDate)))
        }.toSeq
    QlmIDType(
      id,
        (
          idTypeAttr ++ 
          tagTypeAttr ++ 
          startDateAttr ++
          endDateAttr
        ).toMap ++ attributesToDataRecord( attributes )
    )
  }
}
