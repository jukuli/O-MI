
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 +    Copyright (c) 2015 Aalto University.                                        +
 +                                                                                +
 +    Licensed under the 4-clause BSD (the "License");                            +
 +    you may not use this file except in compliance with the License.            +
 +    You may obtain a copy of the License at top most directory of project.      +
 +                                                                                +
 +    Unless required by applicable law or agreed to in writing, software         +
 +    distributed under the License is distributed on an "AS IS" BASIS,           +
 +    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    +
 +    See the License for the specific language governing permissions and         +
 +    limitations under the License.                                              +
 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

package responses
import scala.xml.XML

import database._
import parsing.xmlGen.{defaultScope, scalaxb, xmlTypes}
import types.OdfTypes._
import types.odf._
import http.{ActorSystemContext, Storages}

sealed trait RESTRequest{def path: Path} // path is OdfNode path
case class ValueRequest(path: Path)      extends RESTRequest
case class MetaDataRequest(path: Path)   extends RESTRequest
case class DescriptionRequest(path: Path)extends RESTRequest
case class ObjId(path: Path)      extends RESTRequest
case class InfoName(path: Path)   extends RESTRequest
case class NodeReq(path: Path)    extends RESTRequest

object RESTRequest{
    def apply(path: Path): RESTRequest = path.lastOption match {
      case attr @ Some("value")      => ValueRequest(path.init)
      case attr @ Some("MetaData")   => MetaDataRequest(path.init)
      case attr @ Some("description")=> DescriptionRequest(path.init)
      case attr @ Some("id")         => ObjId(path.init)
      case attr @ Some("name")       => InfoName(path.init)
      case _                         => NodeReq(path)
    }
}
object RESTHandler{


  /**
   * Generates ODF containing only children of the specified path's (with path as root)
   * or if path ends with "value" it returns only that value.
   *
   * @param orgPath The path as String, elements split by a slash "/"
   * @return Some if found, Left(string) if it was a value and Right(xml.Node) if it was other found object.
   */
  def handle(orgPath: Path)(implicit singleStores: SingleStores): Option[Either[String, xml.NodeSeq]] = {
    handle( RESTRequest(orgPath) )
  }
  /**
   * Generates ODF containing only children of the specified path's (with path as root)
   * or if path ends with "value" it returns only that value.
   *
   * @return Some if found, Left(string) if it was a value and Right(xml.Node) if it was other found object.
   */
  def handle(request: RESTRequest)(implicit singleStores: SingleStores): Option[Either[String, xml.NodeSeq]] = {
    request match {
      case ValueRequest(path) =>
        singleStores.latestStore execute LookupSensorData(path) map { Left apply _.value.toString }

      case MetaDataRequest(path) =>
        singleStores.getMetaData(path) map { metaData =>
          Right(scalaxb.toXML[xmlTypes.MetaDataType](metaData.asMetaDataType, Some("odf"), Some("MetaData"),defaultScope))
        }
      case ObjId(path) =>{  //should this query return the id as plain text or inside Object node?
        val xmlReturn = (singleStores.hierarchyStore execute GetTree() ).get( path )map{
          case obj: Object =>
            scalaxb.toXML[xmlTypes.ObjectType](
              obj.copy( descriptions = Vector()).asObjectType(Vector(), Vector()),
              Some("odf"),
              Some("Object"),
              defaultScope
            ).headOption.getOrElse(
              <error>Could not create from OdfObject </error>
            )
          case odfObjs: Objects => <error>Id query not supported for root Object</error>
          case odfInfoItem: InfoItem => <error>Id query not supported for InfoItem</error>
          case _ => <error>Matched default case. The impossible happened?</error>
        }

        xmlReturn map Right.apply
        //Some(Right(<Object xmlns="odf.xsd"><id>{path.last}</id></Object>)) // TODO: support for multiple id
      }
      case InfoName(path) =>
        Some(Right(<InfoItem xmlns="odf.xsd" name={path.last}><name>{path.last}</name></InfoItem>))
        // TODO: support for multiple name
      case DescriptionRequest(path) =>
        val desc = (singleStores.hierarchyStore execute GetTree()).get(path).collect{
          case obj: Object => obj.descriptions
          case ii: InfoItem => ii.descriptions
        }.toSeq.flatten.flatMap{ 
          case desc: Description =>
            scalaxb.toXML[xmlTypes.DescriptionType](
              desc.asDescriptionType, Some("odf"), Some("description"), defaultScope
            )
        }
        if( desc.nonEmpty ){
          Some( Right( desc ))
        } else {
          Some( Right(
           <error>No description found for {path.toString}</error>
         ))
        }
        
      case NodeReq(path) =>
        val odf = (singleStores.hierarchyStore execute GetTree() )
        val xmlReturn = odf.get(path) map {

          case obj: Object =>
            val childs = odf.getChilds( obj.path)
            val (objs, iis) = childs.partition{
              case ii: InfoItem => true
              case o: Object => false
            }
            val iiTypes = iis.collect{ case ii: InfoItem => ii.copy( descriptions = Vector(), metaData = None, values = Vector(), name = Vector()).asInfoItemType }
            val objTypes = objs.collect{ case o: Object => o.copy( descriptions = Vector()).asObjectType(Vector(),Vector()) }

            scalaxb.toXML[xmlTypes.ObjectType](
              obj.asObjectType( iiTypes, objTypes), Some("odf"), Some("Object"), defaultScope
            ).headOption.getOrElse(
              <error>Could not create from OdfObject </error>
            )

          case objs: Objects =>
            val childs = odf.getChilds( objs.path).collect{ case o: Object => o.copy( descriptions = Vector()).asObjectType( Vector(), Vector())}
            scalaxb.toXML[xmlTypes.ObjectsType](
              objs.asObjectsType( childs), Some("odf"), Some("Objects"), defaultScope
            ).headOption.getOrElse(
              <error>Could not create from OdfObjects </error>
            )

          case infoitem: InfoItem =>
            scalaxb.toXML[xmlTypes.InfoItemType](
              infoitem.asInfoItemType, Some("odf"), Some("InfoItem"), defaultScope
            ).headOption.getOrElse(
              <error>Could not create from OdfInfoItem</error>
            )
          case _ => <error>Matched default case. The impossible happened?</error>
        }

        xmlReturn map Right.apply
    }
  }
}
