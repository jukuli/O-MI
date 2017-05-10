package types
package odf

import scala.collection.{ Seq, Map, SortedSet }
import scala.collection.immutable.{TreeSet => ImmutableTreeSet, HashMap => ImmutableHashMap }
import scala.collection.mutable.{TreeSet => MutableTreeSet, HashMap => MutableHashMap }
import scala.xml.NodeSeq
import parsing.xmlGen.xmlTypes.{ObjectsType, ObjectType}
import parsing.xmlGen.{odfDefaultScope, scalaxb, defaultScope}

class MutableODF(
  _nodes: Seq[Node]  = Vector.empty
) extends ODF[MutableHashMap[Path,Node],MutableTreeSet[Path]] {
  type M = MutableHashMap[Path,Node]
  type S = MutableTreeSet[Path]
  protected[odf] val nodes: MutableHashMap[Path,Node] = MutableHashMap.empty
  protected[odf] val paths: MutableTreeSet[Path] = MutableTreeSet( nodes.keys.toSeq:_* )(PathOrdering)
  addNodes( _nodes )
  def union( that: ODF[M,S]): ODF[M,S] = {
    val pathIntersection: SortedSet[Path] = this.paths.intersect( that.paths)
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
                throw new Exception( 
                  "Found two different types in same Path when tried to create union." 
                )
            }
          case ( t, o) => t.orElse( o) 
        }
    }.toSet
    val allPaths = paths ++ that.paths
    val allNodes = thatOnlyNodes ++ intersectingNodes
    this.nodes ++= allNodes.map{ node => node.path -> node }
    this
  }
  def removePaths( removedPaths: Iterable[Path]) : ODF[M,S] = {
    val subtrees = removedPaths.flatMap( getSubTreePaths( _ ) )
    this.nodes --=( subtrees )
    this.paths --=( subtrees )
    this
  } 
  def immutable: ImmutableODF = new ImmutableODF( 
      this.nodes.values.toVector
  )
  //Should be this? or create a copy?
  def mutable: MutableODF =  new MutableODF( 
      this.nodes.values.toVector
  )

  def removePath( path: Path) : ODF[M,S] ={
    val subtreeP = getSubTreePaths( path )
    this.nodes --=( subtreeP )
    this.paths --=( subtreeP )
    this
  }

  def add( node: Node) : ODF[M,S] ={
    if( !nodes.contains( node.path ) ){
      nodes( node.path) = node
      paths += node.path
      if( !nodes.contains(node.path.init) ){
        this.add( node.createParent )
      }
    } else {
      (nodes.get(node.path), node ) match{
        case (Some(old:Object), obj: Object ) =>
          nodes( node.path) = old.union(obj)
        case (Some(old:Objects), objs: Objects ) =>
          nodes( node.path) = old.union(objs)
        case (Some(old:InfoItem), iI: InfoItem ) => 
          nodes( node.path) = old.union(iI)
        case (old, n ) => 
          throw new Exception(
            "Found two different types in same Path when tried to add a new node" 
          )
      }
    }
    this
  }
  
  def addNodes( nodesToAdd: Seq[Node] ) : ODF[M,S] ={
    nodesToAdd.foreach{
      case node: Node =>
        this.add( node )
    }
    this
  }
  def getSubTreeAsODF( path: Path): ODF[M,S] = {
    val subtree: Seq[Node] = getSubTree( path)
    val ancestors: Seq[Node] = path.getAncestors.flatMap{
      case ap: Path =>
        nodes.get(ap)
    }
    new MutableODF(
        (subtree ++ ancestors).toVector
    )
  }

}

