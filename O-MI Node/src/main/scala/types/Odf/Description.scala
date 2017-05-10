package types
package odf

import parsing.xmlGen.scalaxb.DataRecord
import parsing.xmlGen.xmlTypes._
case class  Description(
  val text: String,
  val language: Option[String]
) {

  implicit def asDescriptionType : DescriptionType ={
    DescriptionType(
      text, 
      language.fold(Map.empty[String, DataRecord[Any]]){
        n=>Map( ("@lang" -> DataRecord(n)) ) 
      }
    )
  }

}
