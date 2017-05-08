
import parsing.xmlGen.xmlTypes
import parsing.xmlGen.scalaxb._
import java.util.GregorianCalendar
import javax.xml.datatype.XMLGregorianCalendar
import javax.xml.datatype.DatatypeFactory
import java.sql.Timestamp

package object odf {
  trait Unionable[T] { 
    def union(t: T): T 
  }
 def timestampToXML(timestamp: Timestamp) : XMLGregorianCalendar ={
   val cal = new GregorianCalendar()
   cal.setTime(timestamp)
   DatatypeFactory.newInstance().newXMLGregorianCalendar(cal)
 }

 def attributesToDataRecord( attributes: scala.collection.Map[String,String] ): Map[String,DataRecord[String]] ={
   attributes.map{
      case ( key: String, value: String ) =>
        if( key.startsWith("@") ) key -> DataRecord(value)
        else  s"@$key" -> DataRecord(value)
   }.toMap
 }
}
