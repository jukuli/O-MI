package odf

import scala.collection.{ Seq, Map }

trait Node{
  def createAncestors: Seq[Node] 
  def attributes: Map[String,String]
  def parentPath: Option[Path]
}
