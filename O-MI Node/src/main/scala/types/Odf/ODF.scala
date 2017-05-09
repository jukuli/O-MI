package types
package odf

import scala.collection.{ Seq, Map, SortedSet }
import scala.collection.immutable.{TreeSet, HashMap}
import scala.xml.NodeSeq
import parsing.xmlGen.xmlTypes.{ObjectsType, ObjectType}
import parsing.xmlGen.{odfDefaultScope, scalaxb, defaultScope}

case class ODF(
  val nodes: HashMap[Path,Node] = HashMap.empty
){
  val paths: SortedSet[Path] = TreeSet( nodes.keys.toSeq:_* )(PathOrdering)

  def union( that: ODF ): ODF = {
    val pathIntersection: SortedSet[Path] = this.paths.intersect( that.paths)
    val thisOnlyNodes: Set[Node] = (paths -- pathIntersection ).flatMap{
      case p: Path =>
        nodes.get(p)
    }.toSet
    val thatOnlyNodes: Set[Node] = (that.paths -- pathIntersection ).flatMap{
      case p: Path =>
        that.nodes.get(p)
    }.toSet
    val intersectingNodes: Set[Node] = pathIntersection.flatMap{
      case path: Path =>
        (this.nodes.get(path), that.nodes.get(path) ) match{
          case ( Some( node: Node ), Some( otherNode: Node ) ) =>
            (node, otherNode ) match{
              case (  ii: InfoItem , oii: InfoItem  ) => 
                Some( ii.union(oii) )
              case ( obj: Object ,  oo: Object ) =>
                Some( obj.union( oo ) )
              case ( obj: Objects ,  oo: Objects ) =>
                Some( obj.union( oo ) )
              case ( n, on) => 
                throw new Exception( "Found two unmatching types in same Path when tried to create union." )
            }
          case ( t, o) => t.orElse( o) 
        }
    }.toSet
    val allPaths = paths ++ that.paths
    val allNodes = thisOnlyNodes ++ thatOnlyNodes ++ intersectingNodes
    this.copy( 
      HashMap(
        allNodes.map{
          case node: Node =>
            node.path -> node 
        }.toSeq:_*
      )
    )
  }
  def getInfoItems: Map[Path,InfoItem] = nodes.collect{ 
    case (p: Path, ii: InfoItem) => p -> ii
  }
  def getObjects: Map[Path,Object] = nodes.collect{ 
    case (p: Path, obj: Object) => p -> obj
  }
  def get( path: Path): Option[Node] = nodes.get(path)
  def getSubTree( path: Path): Seq[Node] = {
    (
      nodes.get(path) ++ 
      paths
        .iteratorFrom(path)
        .takeWhile{ case p: Path => path.isAncestorOf(p) }
        .flatMap{   case p: Path => nodes.get(p) }
    ).toVector
  }
  def getChilds( path: Path): Seq[Node] = {
    paths
      .iteratorFrom(path)
      .takeWhile{ case p: Path => path.isAncestorOf(p) }
      .filter{    case p: Path => path.isParentOf(p) }
      .flatMap{   case p: Path => nodes.get(p) }.toVector
  }
  
  def removePaths( removedPaths: Iterable[Path] ) ={
    ODF(
      nodes -- removedPaths
    )
  }

  def --( removedPaths: Iterable[Path] ) ={
    ODF(
      nodes -- removedPaths
    )
  }



  implicit def asObjectsType : ObjectsType ={
    val firstLevelObjects= getChilds( new Path("Objects") )
    val objectTypes= firstLevelObjects.map{
      case obj: Object => 
        createObjectType( obj )
    }
    nodes.get(new Path("Objects")).collect{
      case objs: Objects =>
        objs.asObjectsType(objectTypes) 
    }.getOrElse{
      (new Objects()).asObjectsType(objectTypes) 
    }
  }

  def createObjectType( obj: Object ): ObjectType ={
    val (objects, infoItems ) = getChilds( obj.path ).partition{
      case obj: Object => true
      case ii: InfoItem => false
    }
    obj.asObjectType(
      infoItems.collect{
        case ii: InfoItem => ii.asInfoItemType 
      },
      objects.collect{
        case obj: Object =>
          createObjectType( obj ) 
      }
    )
  }
  implicit def asXML : NodeSeq= {
    val xml  = scalaxb.toXML[ObjectsType](asObjectsType, None, Some("Objects"), odfDefaultScope)
    xml//.asInstanceOf[Elem] % new UnprefixedAttribute("xmlns","odf.xsd", Node.NoAttributes)
  }
}
