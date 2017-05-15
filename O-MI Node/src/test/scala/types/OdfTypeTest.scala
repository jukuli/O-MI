package types 
package odf

import java.util.Date
import java.sql.Timestamp

import scala.xml.Utility.trim
import org.specs2.matcher._
import org.specs2.matcher.XmlMatchers._

import org.specs2._
import types.OdfTypes._
import types.OdfTypes.{QlmID =>OdfQlmID}

class OdfTypesTest extends mutable.Specification{
  val testTime : Timestamp = Timestamp.valueOf("2017-05-11 15:44:55")
  sequential
  "ImmubtableODF" should {
    val o_df = ImmutableODF( testingNodes )
    "create all given paths and their ancestors" in createCorrect(o_df)
    "return all SubTree paths when asked" in getCorrectSubTree(o_df)
    "create correct XML presentation" in toXMLTest(o_df)
    "add also missing ancestors when a Node is added" in addTest(o_df) 
  }
  "MubtableODF" should {
    val o_df = MutableODF( testingNodes )
    "create all given paths and their ancestors" in createCorrect(o_df) 
    "return all SubTree paths when asked" in getCorrectSubTree(o_df)
    "create correct XML presentation" in toXMLTest(o_df)
    "add also missing ancestors when a Node is added" in addTest(o_df) 
  }
  "OMIParser" should {
    val o_df = ImmutableODF( testingNodes )
    "should parse correctly XML created from ODF to equal ODF" in fromXMLTest(o_df) 
  } 
  "Converters" should {
    "convert from old to new and back to old and stay the same" in repeatedOldConvertTest
    "convert from new to old and back to new and stay the same" in repeatedNewConvertTest
  }
  def  repeatedNewConvertTest ={
    val newType = ImmutableODF( testingNodes )
    val oldType = NewTypeConverter.convertODF( newType )
    val backToNew = OldTypeConverter.convertOdfObjects( oldType )
    val o = backToNew
    val iODF = newType 
    lazy val parsedPaths = o.getPaths.toSet 
    lazy val correctPaths = iODF.getPaths.toSet
    lazy val pathCheck = (parsedPaths must contain( correctPaths ) ) and
    ( (parsedPaths -- correctPaths) must beEmpty ) and ( (correctPaths -- parsedPaths) must beEmpty )
    lazy val parsedII = o.getInfoItems.toSet 
    lazy val correctII = iODF.getInfoItems.toSet
    lazy val iICheck ={
      (parsedII -- correctII ) must beEmpty } and {
      (correctII -- parsedII) must beEmpty } and {
      parsedII must contain(correctII)
    } 

    lazy val parsedObj = o.getObjects.toSet 
    lazy val correctObj = iODF.getObjects.toSet
    lazy val objCheck ={
      (parsedObj -- correctObj ) must beEmpty } and {
      (correctObj -- parsedObj ) must beEmpty } and {
      parsedObj must contain(correctObj)
    } 

    lazy val parsedMap = o.getNodesMap
    lazy val correctMap = iODF.getNodesMap
    lazy val mapCheck = parsedMap.toSet must contain(correctMap.toSet)
  
    pathCheck and iICheck and objCheck and mapCheck //and (backToNew must be(newType)) //TODO: Why matching ditrecly fails?
  }


  def  repeatedOldConvertTest ={
    val oldType : OdfObjects = parsing.OdfParser.parse( testingNodesAsXML.toString ) match {
      case Right( o ) => o
      case Left( errors ) =>
        println( errors )
        throw new Exception("Parsing failed!")
    }
    val newType = OldTypeConverter.convertOdfObjects(oldType) 
    val backToOld = NewTypeConverter.convertODF(newType)
    val p = new scala.xml.PrettyPrinter(120, 4)
    backToOld.asXML showAs{
      case ns => 
        "Original:\n\n" + p.format(oldType.asXML.head) + "\n\n" ++
        "Converted:\n\n" + p.format(ns.head) + "\n"
      
    } must beEqualToIgnoringSpace(
      oldType.asXML     
    )
  }

  def createCorrect[M<:scala.collection.Map[Path,Node],S <: scala.collection.SortedSet[Path]](
    o_df: ODF[M,S]
  ) = {
    val iIPaths = testingNodes.collect{ 
      case iI: InfoItem => iI.path
    }.toSet
    val objPaths = testingNodes.collect{ 
      case obj: Object => obj.path
    }.toSet
    val automaticallyCreatedPaths = Set(
      Path("Objects/ObjectA"),
      Path("Objects/ObjectB"),
      Path("Objects/ObjectB/ObjectB"),
      Path("Objects/ObjectC")
    )
    val createdIIPaths = o_df.getInfoItems.map( _.path).toSet 
    val createdObjPaths = o_df.getObjects.map( _.path).toSet
    (createdIIPaths should contain(iIPaths) )and ( 
    createdObjPaths should contain(objPaths ++ automaticallyCreatedPaths) )
  }
  def getCorrectSubTree[M<:scala.collection.Map[Path,Node],S <: scala.collection.SortedSet[Path]](
    o_df: ODF[M,S]
  ) = {
    o_df.getSubTreePaths( Path("Objects/ObjectA")).toSet should contain(
      Set(
        Path("Objects/ObjectA"),
        Path("Objects/ObjectA/II1"),
        Path("Objects/ObjectA/II2")
      )
    )
  }
  def addTest[M<:scala.collection.Map[Path,Node],S <: scala.collection.SortedSet[Path]](
    o_df: ODF[M,S]
  ) = {
    val beAdded = InfoItem( 
        "II1",
        Path("Objects/ObjectN/SubObj/II1"),
        Vector(
          QlmID(
            "II2O1",
            Some("TestID"),
            Some("TestTag")
          ),
          QlmID(
            "II2O2",
            Some("TestID"),
            Some("TestTag")
          )
        ),
        testingDescription,
        Vector(
          Value( 93, testTime, testingAttributes ), 
          Value( "51.9", "xs:float", testTime ), 
          Value( "51.9", testTime, testingAttributes)
        ),
        Some(MetaData(
          Vector(
          InfoItem( 
            "A",
            Path("Objects/ObjectA/II2/MetaData/A")
          ),
          InfoItem( 
            "B",
            Path("Objects/ObjectA/II2/MetaData/B")
          ))
        )),
        testingAttributes
      )
    o_df.add( beAdded ).getSubTreePaths( 
      Path("Objects/ObjectN")
    ).toSet should contain(
      Set(
        Path("Objects/ObjectN"),
        Path("Objects/ObjectN/SubObj"),
        Path("Objects/ObjectN/SubObj/II1")
      )
    )
  }
  def toXMLTest[M<:scala.collection.Map[Path,Node],S <: scala.collection.SortedSet[Path]](
    o_df: ODF[M,S]
  ) = {
    val p = new scala.xml.PrettyPrinter(120, 4)
    o_df.asXML showAs{
      case ns => 
        "Generated:\n\n" + p.format(ns.head) + "\n"
      
    } must beEqualToIgnoringSpace( testingNodesAsXML )
  }
  def fromXMLTest[M<:scala.collection.Map[Path,Node],S <: scala.collection.SortedSet[Path]](
    o_df: ODF[M,S]
  ) ={
    ODFParser.parse( o_df.asXML.toString ) should beRight{
      o: ImmutableODF => 
        val iODF = o_df.immutable 
        lazy val parsedPaths = o.getPaths.toSet 
        lazy val correctPaths = iODF.getPaths.toSet
        lazy val pathCheck = (parsedPaths must contain( correctPaths ) ) and
        ( (parsedPaths -- correctPaths) must beEmpty ) and ( (correctPaths -- parsedPaths) must beEmpty )
        lazy val parsedII = o.getInfoItems.toSet 
        lazy val correctII = iODF.getInfoItems.toSet
        lazy val iICheck ={
         (parsedII -- correctII ) must beEmpty } and {
          (correctII -- parsedII) must beEmpty } and {
          parsedII must contain(correctII)
        } 
        lazy val parsedObj = o.getObjects.toSet 
        lazy val correctObj = iODF.getObjects.toSet
        lazy val objCheck ={
         (parsedObj -- correctObj ) must beEmpty } and {
          (correctObj -- parsedObj ) must beEmpty } and {
          parsedObj must contain(correctObj)
        } 
        lazy val parsedMap = o.getNodesMap
        lazy val correctMap = iODF.getNodesMap
        lazy val mapCheck = parsedMap.toSet must contain(correctMap.toSet)

        pathCheck and iICheck and objCheck and mapCheck //and (o must be( iODF )) //TODO: Why matching ditrecly fails?
    }
  }
 
  def testingNodesAsXML ={
  <Objects xmlns="http://www.opengroup.org/xsd/odf/1.0/">
        <Object>
            <id>ObjectA</id>
            <InfoItem name="II1">
                <name>II1</name>
            </InfoItem>
            <InfoItem name="II2">
                <name>II2</name>
                <name tagType="TestTag" idType="TestID">II2O1</name>
                <name tagType="TestTag" idType="TestID">II2O2</name>
                <description lang="English">Testing</description>
                <description lang="Finnish">Testaus</description>
                <MetaData>
                    <InfoItem name="A">
                        <name>A</name>
                    </InfoItem>
                    <InfoItem name="B">
                        <name>B</name>
                    </InfoItem>
                </MetaData>
                <value unixTime="1494506695" type="xs:int" dateTime="2017-05-11T15:44:55.000+03:00">93</value>
                <value unixTime="1494506695" type="xs:float" dateTime="2017-05-11T15:44:55.000+03:00">51.9</value>
                <value unixTime="1494506695" dateTime="2017-05-11T15:44:55.000+03:00">81.5</value>
            </InfoItem>
        </Object>
        <Object>
            <id>ObjectB</id>
            <Object>
                <id>ObjectB</id>
                <InfoItem name="II1">
                    <name>II1</name>
                </InfoItem>
            </Object>
        </Object>
        <Object>
            <id>ObjectC</id>
            <Object type="TestingType">
                <id tagType="TestTag" idType="TestID">ObjectCC</id>
                <id tagType="TestTag" idType="TestID">OCC</id>
                <description lang="English">Testing</description>
                <description lang="Finnish">Testaus</description>
            </Object>
        </Object>
    </Objects>
  }

  def testingNodes: Vector[Node] = Vector(
      InfoItem( 
        "II1",
        Path("Objects/ObjectA/II1"),
        Vector( QlmID( "II1" ))
      ),
      InfoItem( 
        "II2",
        Path("Objects/ObjectA/II2"),
        Vector(
          QlmID( "II2" ),
          QlmID(
            "II2O1",
            Some("TestID"),
            Some("TestTag")
          ),
          QlmID(
            "II2O2",
            Some("TestID"),
            Some("TestTag")
          )
        ),
        testingDescription,
        Vector(
          Value( 93, testTime), 
          Value( "51.9", "xs:float", testTime ), 
          Value( "81.5", testTime)
        ),
        Some(MetaData(
          Vector(
          InfoItem( 
            "A",
            Path("Objects/ObjectA/II2/MetaData/A"),
            Vector( QlmID( "A" ))
          ),
          InfoItem( 
            "B",
            Path("Objects/ObjectA/II2/MetaData/B"),
            Vector( QlmID( "B" ))
          ))
        ))
      ),
      InfoItem( 
        "II1",
        Path("Objects/ObjectB/ObjectB/II1"),
        Vector( QlmID( "II1" ))
      ),
      Object(
        Vector(
          QlmID(
            "ObjectCC",
            Some("TestID"),
            Some("TestTag")
          ),
          QlmID(
            "OCC",
            Some("TestID"),
            Some("TestTag")
          )
        ),
        Path("Objects/ObjectC/ObjectCC"),
        Some("TestingType"),
        testingDescription
     )
    )

  def testingAttributes = Map{
    "testing" -> "true"
  }
  def testingDescription =Vector(
    Description(
      "Testing",
      Some("English")
    ),
    Description(
      "Testaus",
      Some("Finnish")
    )
  )

  val oldodf: OdfObjects = {
    /*Right(
      Vector(
        WriteRequest(
          10.0, */ OdfObjects(
      Vector(
        OdfObject(
        Vector(),
          types.Path("Objects/SmartHouse"), Vector(
            OdfInfoItem(
              types.Path("Objects/SmartHouse/PowerConsumption"), Vector(
                OdfValue(
                  "180", "xs:string",
                    Timestamp.valueOf("2014-12-18 15:34:52"))), None, None), OdfInfoItem(
              types.Path("Objects/SmartHouse/Moisture"), Vector(
                OdfValue(
                  "0.20", "xs:string",
                    new Timestamp(1418916892L * 1000))), None, None)), Vector(
            OdfObject(
            Vector(),
              types.Path("Objects/SmartHouse/SmartFridge"), Vector(
                OdfInfoItem(
                  types.Path("Objects/SmartHouse/SmartFridge/PowerConsumption"), Vector(
                    OdfValue(
                      "56", "xs:string",
                        Timestamp.valueOf("2014-12-18 15:34:52"))), None, None)), Vector(), None, None), OdfObject(
            Vector(),
              types.Path("Objects/SmartHouse/SmartOven"), Vector(
                OdfInfoItem(
                  types.Path("Objects/SmartHouse/SmartOven/PowerOn"), Vector(
                    OdfValue(
                      "1", "xs:string",
                        Timestamp.valueOf("2014-12-18 15:34:52"))), None, None)), Vector(), None, None)), None, None), OdfObject(
        Vector(),
          types.Path("Objects/SmartCar"), Vector(
            OdfInfoItem(
              types.Path("Objects/SmartCar/Fuel"),
              Vector(OdfValue(
                  "30",
                  "xs:string",
                  Timestamp.valueOf("2014-12-18 15:34:52")
              )), 
              None, 
              Some(OdfMetaData(
                Vector(OdfInfoItem(
                  types.Path("Objects/SmartCar/Fuel/MetaData/Units"),
                  Vector(OdfValue(
                    "Litre",
                    "xs:string",
                    Timestamp.valueOf("2014-12-18 15:34:52")
                  ))
                ))
              ))
            )),
          Vector(), None, None), OdfObject(
        Vector(),
          types.Path("Objects/SmartCottage"), Vector(), Vector(
            OdfObject(
            Vector(),
              types.Path("Objects/SmartCottage/Heater"), Vector(), Vector(), None, None), OdfObject(
            Vector(),
              types.Path("Objects/SmartCottage/Sauna"), Vector(), Vector(), None, None), OdfObject(
            Vector(),
              types.Path("Objects/SmartCottage/Weather"), Vector(), Vector(), None, None)), None, None)), None)
  }
}
