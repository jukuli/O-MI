package types
package odf

import scala.collection.{ Seq, Map, SortedSet }
import scala.collection.immutable.{TreeSet => ImmutableTreeSet, HashMap => ImmutableHashMap }
import scala.collection.mutable.{TreeSet => MutableTreeSet, HashMap => MutableHashMap }
import scala.xml.NodeSeq
import parsing.xmlGen.xmlTypes.{ObjectsType, ObjectType}
import parsing.xmlGen.{odfDefaultScope, scalaxb, defaultScope}

case class ImmutableODF private[odf] (
  _nodes: Seq[Node]  = Vector.empty
) extends ODF[ImmutableHashMap[Path,Node],ImmutableTreeSet[Path]] {
  type M = ImmutableHashMap[Path,Node]
  type S = ImmutableTreeSet[Path]
  protected[odf] val nodes: ImmutableHashMap[Path,Node] = {
    val tmpMutable = new MutableODF()
    val sorted = _nodes.sortBy( _.path)(PathOrdering)
    sorted.foreach{
      case node: Node =>
        tmpMutable.add( node )
    }
    ImmutableHashMap(tmpMutable.nodes.toSeq:_*)
  }
  protected[odf] val paths: ImmutableTreeSet[Path] = ImmutableTreeSet( nodes.keys.toSeq:_* )(PathOrdering)

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

  def removePaths( removedPaths: Iterable[Path]) : ODF[M,S] = {
    this.mutable.removePaths( removedPaths ).immutable
  }
  
  def removePath( path: Path) : ODF[M,S] ={
    this.mutable.removePath( path ).immutable
  }

  def add( node: Node ) : ODF[M,S] ={
    this.mutable.add( node ).immutable
  }

  def immutable: ImmutableODF = this
  def mutable: MutableODF = new MutableODF( 
      nodes.values.toVector
  )

  def addNodes( nodesToAdd: Seq[Node] ) : ODF[M,S] ={
    this.mutable.addNodes( nodesToAdd).immutable
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
}
