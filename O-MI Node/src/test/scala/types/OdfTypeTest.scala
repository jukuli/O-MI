package types 
package odf

import java.util.Date
import java.sql.Timestamp

import scala.xml.Utility.trim
import org.specs2.matcher._
import org.specs2.matcher.XmlMatchers._

import org.specs2._

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
  ) ={
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
        o must be( o_df.immutable)
    }
  }
 
  def testingNodesAsXML ={
    <Objects xmlns="http://www.opengroup.org/xsd/odf/1.0/">
      <Object>
        <id>ObjectA</id>
        <InfoItem name="II1"/>
        <InfoItem name="II2">
          <name idType="TestID" tagType="TestTag">IIO1</name>
          <name idType="TestID" tagType="TestTag">IIO2</name>
          <description lang="English">Testing</description>
          <description lang="Finnish">Testaus</description>
          <MetaData>
            <InfoItem name="A"/>
            <InfoItem name="B"/>
          </MetaData>
          <value type="xs:int" dateTime={s"${timestampToXML(testTime).toXMLFormat()}"} unixTime={s"${testTime.getTime / 1000}"} >{93}</value>
          <value type="xs:float" dateTime={s"${timestampToXML(testTime).toXMLFormat()}"} unixTime={s"${testTime.getTime / 1000}"} >{51.9}</value>
          <value dateTime={s"${timestampToXML(testTime).toXMLFormat()}"} unixTime={s"${testTime.getTime / 1000}"} >{81.5}</value>
        </InfoItem>
      </Object>
      <Object>
        <id>ObjectB</id>
        <Object>
          <id>ObjectB</id>
          <InfoItem name="II1"/>
        </Object>
      </Object>
      <Object>
        <id>ObjectC</id>
        <Object type="TestingType">
          <name idType="TestID" tagType="TestTag">ObjectCC</name>
          <name idType="TestID" tagType="TestTag">OCC</name>
          <description lang="English">Testing</description>
          <description lang="Finnish">Testaus</description>
        </Object>
      </Object>
    </Objects>
  }

  def testingNodes: Seq[Node] = Vector(
      InfoItem( 
        "II1",
        Path("Objects/ObjectA/II1")
      ),
      InfoItem( 
        "II2",
        Path("Objects/ObjectA/II2"),
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
          Value( 93, testTime), 
          Value( "51.9", "xs:float", testTime ), 
          Value( "81.5", testTime)
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
        ))
      ),
      InfoItem( 
        "II1",
        Path("Objects/ObjectB/ObjectB/II1")
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

}
