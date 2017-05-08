package odf

import scala.collection.{ Seq, Map, SortedSet }

trait ODF{
  def nodes: Map[Path,Node]
  def paths: SortedSet[Path]

  def copy( nodes: Map[Path,Node] ): ODF
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
      allNodes.flatMap{
        case node: Node =>
          if( node.parentPath.isEmpty ) 
            throw new Exception( "Found odf.Node without parentPath when unioning.")
          node.parentPath.map{
            p => p -> node 
          }
      }.toMap 
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
  implicit def asObjectsType : ObjectsType ={
    val firstLevelObjects= getChilds( new Path("Objects") )
    firstLevelObjects.map{
      case obj: Object => 
        createObjectType( obj )
    }
  }

  def createObjectType( obj: Object ) ={
    val (objects, infoItems ) = getChilds( obj.path ).partition{
      case obj: Object => true
      case ii: InfoItem => false
    }
    obj.asObjectType(
      infoItems.map( _.asInfoItemsType ),
      objects.map( createObjectType( _ ) )
    )
  }
}
