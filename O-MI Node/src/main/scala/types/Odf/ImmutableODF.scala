package types
package odf

import scala.collection.{ Seq, Map, SortedSet }
import scala.collection.immutable.{TreeSet => ImmutableTreeSet, HashMap => ImmutableHashMap }
import scala.collection.mutable.{TreeSet => MutableTreeSet, HashMap => MutableHashMap }
import scala.xml.NodeSeq
import parsing.xmlGen.xmlTypes.{ObjectsType, ObjectType}
import parsing.xmlGen.{odfDefaultScope, scalaxb, defaultScope}

case class ImmutableODF private[odf] (
  protected[odf] val nodes: ImmutableHashMap[Path,Node] 
) extends ODF[ImmutableHashMap[Path,Node],ImmutableTreeSet[Path]] {

  type GeneralODF = ODF[scala.collection.Map[Path,Node], scala.collection.SortedSet[Path] ]
  type M = ImmutableHashMap[Path,Node]
  type S = ImmutableTreeSet[Path]

  protected[odf] val paths: ImmutableTreeSet[Path] = ImmutableTreeSet( nodes.keys.toSeq:_* )(PathOrdering)
  def update( that: GeneralODF ): ImmutableODF ={
    ImmutableODF(
    nodes.mapValues{
      case node: Node => 
        (node,that.get(node.path)) match {
          case ( ii: InfoItem, Some( iiu: InfoItem) ) => ii.update(iiu)
          case ( obj: Object, Some( ou: Object)) => obj.update(ou)
          case ( objs: Objects, Some( objsu: Objects)) => objs.update( objsu)
          case ( n, None) => n
          case ( n, Some(a)) => throw new Exception( "Missmatching types in ODF when updating.")
        }
    }.values.toVector
    )
  }


  def getTree( selectingPaths: Seq[Path] ) : ODF[M,S] ={
    val ancestorsPaths = selectingPaths.flatMap{ p => p.getAncestors }.toSet
    val subTreePaths = getSubTreePaths( selectingPaths )
    val nodesSelected = (subTreePaths ++ ancestorsPaths).flatMap{
      path => nodes.get( path )
    }
    ImmutableODF( nodesSelected )
  }
  def union( that: ODF[M,S]): ODF[M,S] = {
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
                throw new Exception( "Found two different types in same Path when tried to create union." )
            }
          case ( t, o) => t.orElse( o) 
        }
    }.toSet
    val allPaths = paths ++ that.paths
    val allNodes = thisOnlyNodes ++ thatOnlyNodes ++ intersectingNodes
    ImmutableODF(
      allNodes.toVector
    )
  }

  def removePaths( pathsToRemove: Iterable[Path]) : ODF[M,S] = {
    val subTrees = pathsToRemove.flatMap{ p => getSubTreePaths(p) }.toSet
    this.copy( nodes --( subTrees ) )
  }
  
  def removePath( path: Path) : ODF[M,S] ={
    val subtreeP = getSubTreePaths( path )
    this.copy( nodes --( subtreeP ) )
  }

  def add( node: Node ) : ODF[M,S] ={
    
    val newNodes: ImmutableHashMap[Path,Node] = if( nodes.contains( node.path ) ){
      (nodes.get(node.path), node ) match{
        case (Some(old:Object), obj: Object ) =>
          nodes.updated( node.path,  old.union(obj) )
        case (Some(old:Objects), objs: Objects ) =>
          nodes.updated( node.path,  old.union(objs) )
        case (Some(old:InfoItem), iI: InfoItem ) => 
          nodes.updated( node.path,  old.union(iI) )
        case (old, n ) => 
          throw new Exception(
            "Found two different types in same Path when tried to add a new node" 
          )
      }
    } else {
      val mutableHMap : MutableHashMap[Path,Node] = MutableHashMap(nodes.toVector:_*)
      var toAdd = node
      while( !mutableHMap.contains(toAdd.path) ){
        mutableHMap += toAdd.path -> toAdd
        toAdd = toAdd.createParent
      }
      ImmutableHashMap( mutableHMap.toVector:_*)
    }
    this.copy( newNodes )
  }

  def getSubTreeAsODF( pathsToGet: Seq[Path]): ODF[M,S] = {
    ImmutableODF(
      nodes.values.filter{
        case node: Node => 
          pathsToGet.exists{
            case filter: Path =>
              filter.isAncestorOf( node.path ) || filter == node.path
          }
      }.toVector
    )
  }
  def intersection( o_df: ODF[M,S] ) : ODF[M,S]={
    val iPaths = this.intersectingPaths(o_df)
    ImmutableODF(
      iPaths.map{
        case path: Path => 
          (nodes.get(path),o_df.nodes.get(path)) match{
            case ( None, _) => throw new Exception( s"Not found element in intersecting path $path" ) 
            case (  _, None) => throw new Exception( s"Not found element in intersecting path $path" )
            case ( Some(ii: InfoItem), Some(oii: InfoItem)) => ii intersection oii
            case ( Some(obj: Object), Some(oObj: Object)) => obj intersection oObj
            case ( Some(objs: Objects), Some(oObjs: Objects)) => objs intersection oObjs
            case ( Some( l), Some( r )) => throw new Exception( s"Found nodes with different types in intersecting path $path" )
          }
      }.toVector
    )
  
  }

  def valuesRemoved: ODF[M,S] = this.copy( ImmutableHashMap( nodes.mapValues{ 
    case ii: InfoItem => ii.copy( values = Vector() )
    case obj: Object => obj 
    case obj: Objects => obj
  }.toVector:_*))
  def descriptionsRemoved: ODF[M,S] = this.copy( ImmutableHashMap( nodes.mapValues{ 
    case ii: InfoItem => ii.copy( descriptions = Vector())
    case obj: Object => obj .copy( descriptions = Vector())
    case obj: Objects => obj
  }.toVector:_*))
  def metaDatasRemoved: ODF[M,S] = this.copy( ImmutableHashMap( nodes.mapValues{ 
    case ii: InfoItem => ii.copy( metaData = None )
    case obj: Object => obj 
    case obj: Objects => obj
  }.toVector:_*))
  def attributesRemoved: ODF[M,S] = this.copy( ImmutableHashMap( nodes.mapValues{ 
    case ii: InfoItem => ii.copy( typeAttribute = None , attributes = ImmutableHashMap())
    case obj: Object => obj .copy(typeAttribute = None , attributes = ImmutableHashMap() )
    case obj: Objects => obj
  }.toVector:_*))
  def immutable: ImmutableODF = this.copy()
  def mutable: MutableODF = MutableODF( 
      nodes.values.toVector
  )

  def addNodes( nodesToAdd: Seq[Node] ) : ODF[M,S] ={
    val mutableHMap : MutableHashMap[Path,Node] = MutableHashMap(nodes.toVector:_*)
    val sorted = nodesToAdd.sortBy( _.path)(PathOrdering)
    sorted.foreach{
      case node: Node =>
        if( mutableHMap.contains( node.path ) ){
            (node, mutableHMap.get(node.path) ) match{
              case (  ii: InfoItem , Some(oii: InfoItem) ) => 
                mutableHMap(ii.path) = ii.union(oii) 
              case ( obj: Object ,  Some(oo: Object) ) =>
                mutableHMap(obj.path) = obj.union( oo ) 
              case ( obj: Objects , Some( oo: Objects) ) =>
                mutableHMap(obj.path) = obj.union( oo ) 
              case ( n, on) => 
                throw new Exception( 
                  "Found two different types for same Path when tried to create ImmutableODF." 
                )
            }
        } else {
          var toAdd = node
          while( !mutableHMap.contains(toAdd.path) ){
            mutableHMap += toAdd.path -> toAdd
            toAdd = toAdd.createParent
          }
        }
    }
    this.copy(
      ImmutableHashMap(
        mutableHMap.toVector:_*
      )
    )
  }
  def getSubTreeAsODF( path: Path): ODF[M,S] = {
    val subtree: Seq[Node] = getSubTree( path)
    val ancestors: Seq[Node] = path.getAncestors.flatMap{
      case ap: Path => nodes.get(ap)
    }
    ImmutableODF(
        (subtree ++ ancestors).toVector
    )
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
}

object ImmutableODF{
  def apply(
      _nodes: Seq[Node]  = Vector.empty
  ) : ImmutableODF ={
    val mutableHMap : MutableHashMap[Path,Node] = MutableHashMap.empty
    val sorted = _nodes.sortBy( _.path)(PathOrdering)
    sorted.foreach{
      case node: Node =>
        if( mutableHMap.contains( node.path ) ){
            (node, mutableHMap.get(node.path) ) match{
              case (  ii: InfoItem , Some(oii: InfoItem) ) => 
                mutableHMap(ii.path) = ii.union(oii) 
              case ( obj: Object ,  Some(oo: Object) ) =>
                mutableHMap(obj.path) = obj.union( oo ) 
              case ( obj: Objects , Some( oo: Objects) ) =>
                mutableHMap(obj.path) = obj.union( oo ) 
              case ( n, on) => 
                throw new Exception( 
                  "Found two different types for same Path when tried to create ImmutableODF." 
                )
            }
        } else {
          var toAdd = node
          while( !mutableHMap.contains(toAdd.path) ){
            mutableHMap += toAdd.path -> toAdd
            toAdd = toAdd.createParent
          }
        }
    }
    new ImmutableODF(
      ImmutableHashMap(
        mutableHMap.toVector:_*
      )
    )
  }
}
