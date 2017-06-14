package types
package odf

import scala.collection.{ Seq, Map, SortedSet }
import scala.collection.immutable.{TreeSet => ImmutableTreeSet, HashMap => ImmutableHashMap }
import scala.collection.mutable.{TreeSet => MutableTreeSet, HashMap => MutableHashMap }
import scala.xml.NodeSeq
import parsing.xmlGen.xmlTypes.{ObjectsType, ObjectType}
import parsing.xmlGen.{odfDefaultScope, scalaxb, defaultScope}

  /** O-DF structure
   */
trait ODF[M <: scala.collection.Map[Path,Node], S<: scala.collection.SortedSet[Path] ]
{
  /** All nodes(InfoItems and Objects) in O-DF structure.
   */
  protected[odf] def nodes : M//= HashMap.empty
  /** SortedSet of all paths in O-DF structure. 
   * Should be ordered by paths with alpabetic ordering so that after a path
   * comes all its descendant: 
   * A/, A/a, A/a/1, A/b/, A/b/1, Aa/ ..
   *
  */
  protected[odf] def paths : S //= TreeSet( nodes.keys.toSeq:_* )(PathOrdering)
  //def copy( nodes : scala.collection.Map[Path,Node] ): ODF[M,S]

  def union( that: ODF[M,S]): ODF[M,S] 
  def removePaths( removedPaths: Iterable[Path]) : ODF[M,S]  
  def immutable: ImmutableODF
  def mutable: MutableODF
  def getInfoItems: Seq[InfoItem] = nodes.collect{ 
    case (p: Path, ii: InfoItem) => ii
  }.toVector
  def getObjects: Seq[Object] = nodes.collect{ 
    case (p: Path, obj: Object) => obj
  }.toVector
  def get( path: Path): Option[Node] = nodes.get(path)
  def getSubTreePaths( pathsToGet: Seq[Path]): Seq[Path] = {
    paths.filter{
      case path: Path => 
        pathsToGet.exists{
          case filter: Path =>
            filter.isAncestorOf( path ) || filter == path
        }
    }.toVector
  }
  def getSubTreePaths( path: Path): Seq[Path] = {
      paths
        .iteratorFrom(path)
        .takeWhile{ case p: Path => path.isAncestorOf(p) || p == path}
        .toVector
  }
  def getSubTree( pathsToGet: Seq[Path]): Seq[Node] = {
    nodes.values.filter{
      case node: Node => 
        pathsToGet.exists{
          case filter: Path =>
            filter.isAncestorOf( node.path ) || filter == node.path
        }
    }.toVector
  }
  def getSubTreeAsODF( pathsToGet: Seq[Path]): ODF[M,S]
  
  def getPaths: Seq[Path] = paths.toVector
  def getNodes: Seq[Node] = nodes.values.toVector
  def getNodesMap: Map[Path,Node] = ImmutableHashMap(
    nodes.toVector:_*
  )
  def getChildPaths( path: Path): Seq[Path] = {
    getSubTreePaths(path).filter{ 
      case p: Path => path.isParentOf(p) 
    }
  }
  def getSubTree( path: Path): Seq[Node] = {
    (
      //nodes.get(path) ++ 
      getSubTreePaths(path).flatMap{   case p: Path => nodes.get(p) }
    ).toVector
  }
  def getChilds( path: Path): Seq[Node] = {
    getChildPaths(path).flatMap{   case p: Path => nodes.get(p) }.toVector
  }

  def --( removedPaths: Iterable[Path] ) : ODF[M,S] = removePaths( removedPaths )
  def removePath( path: Path) : ODF[M,S]
  def add( node: Node ) : ODF[M,S]
  def addNodes( nodesToAdd: Seq[Node] ) : ODF[M,S] 
  def getSubTreeAsODF( path: Path): ODF[M,S]

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

  def getLeafs: Vector[Node] = {
    getLeafPaths.flatMap( nodes.get(_)).toVector
  }
  def getLeafPaths: Set[Path] = {
    val ps = paths.toSeq
    ps.filter{
      path: Path => 
        val index = ps.indexOf( path) 
        val nextIndex = index +1
        if( nextIndex < ps.size ){
          val nextPath: Path = ps(nextIndex) 
          !path.isAncestorOf( nextPath )
        } else true
    }.toSet
  }
  def pathsOfInfoItemsWithMetaData: Set[Path] ={
    nodes.values.collect{
      case ii: InfoItem if ii.metaData.nonEmpty => ii.path
    }.toSet
  }
  def infoItemsWithMetaData: Set[InfoItem] ={
    nodes.values.collect{
      case ii: InfoItem if ii.metaData.nonEmpty => ii
    }.toSet
  }
  def nodesWithDescription: Set[Node] ={
    nodes.values.collect{
      case ii: InfoItem if ii.description.nonEmpty => ii
      case obj: Object if obj.description.nonEmpty => obj
    }.toSet
  }
  def pathsOfNodesWithDescription: Set[Path] ={
    nodes.values.collect{
      case ii: InfoItem if ii.description.nonEmpty => ii.path
      case obj: Object if obj.description.nonEmpty => obj.path
    }.toSet
  }
  def pathsWithType( typeStr: String ): Set[Path] ={
    nodes.values.collect{
      case ii: InfoItem if ii.typeAttribute == Some( typeStr ) => ii.path
      case obj: Object if obj.typeAttribute == Some( typeStr ) => obj.path
    }.toSet
  }

  def valuesRemoved: ODF[M,S]
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
  override def toString: String ={
    "ODF{\n" +
    nodes.map{
      case (p, node) => 
        s"$p --> $node" 
    }.mkString("\n") + "\n}"
  }
  override def equals( that: Any ) : Boolean ={
    that match{
      case another: ODF[M,S] =>
        println( s"Path equals: ${paths equals another.paths}\n Nodes equals:${nodes equals another.nodes}" )
        (paths equals another.paths) && (nodes equals another.nodes)
      case a: Any => 
        println( s" Comparing ODF with something: $a")
        false
    }
  }
  override lazy val hashCode: Int = this.nodes.hashCode
  def intersection( o_df: ODF[M,S] ) : ODF[M,S]
  def intersectingPaths[A <: scala.collection.Map[Path,Node], B<: scala.collection.SortedSet[Path] ]( o_df: ODF[A,B]): SortedSet[Path] ={
    paths.intersect(o_df.paths)
  }
}

object ODF{
  /*
  def apply[M <: scala.collection.Map[Path,Node], S<: scala.collection.SortedSet[Path] ]( 
    nodes: M
  ) : ODF[M,S] ={
    nodes match {
      case mutable: 
    }
  }*/
}
