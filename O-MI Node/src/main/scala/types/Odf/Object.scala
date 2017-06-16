package types
package odf

import scala.collection.{ Seq, Map }
import scala.collection.immutable.{ HashMap, Map =>IMap}

import parsing.xmlGen.scalaxb.DataRecord
import parsing.xmlGen.xmlTypes.{InfoItemType, ObjectType}

case class Object(
  val id: Vector[QlmID],
  val path: Path,
  val typeAttribute: Option[String] = None,
  val descriptions: Seq[Description] = Vector.empty,
  val attributes: IMap[String,String] = HashMap.empty
) extends Node with Unionable[Object] {
  assert( id.nonEmpty )
  assert( path.length >= 2 )
  assert( id.map(_.id).toSet.contains(path.last) )

  def intersection( that: Object ): Object ={
    val pathsMatches = path == that.path 
    val containSameId = id.map( _.id ).toSet.intersect( that.id.map( _.id).toSet ).nonEmpty
    assert( containSameId && pathsMatches)
    new Object(
      if( that.id.nonEmpty ){
        QlmID.unionReduce( that.id ++ id).toVector.filter{ case id => id.id.nonEmpty}
      } else Vector.empty,
      path,
      that.typeAttribute.orElse(typeAttribute),
      if( that.descriptions.nonEmpty ){
        Description.unionReduce(that.descriptions ++ descriptions).toVector.filter{ case desc => desc.text.nonEmpty}
      } else Vector.empty,
      that.attributes ++ attributes
    )
    
  }
  def union( that: Object ): Object ={
    val pathsMatches = path == that.path 
    val containSameId = id.map( _.id ).toSet.intersect( that.id.map( _.id).toSet ).nonEmpty
    assert( containSameId && pathsMatches)
    new Object(
      QlmID.unionReduce(id ++ that.id).toVector,
      path,
      (typeAttribute, that.typeAttribute) match {
        case (Some( t ), Some( ot ) ) =>
          if ( t == ot) Some( t ) 
          else Some( t + " " + ot)
        case (t, ot) => t.orElse(ot)
      },
      Description.unionReduce(descriptions ++ that.descriptions).toVector,
      attributes ++ that.attributes
    )
    
  }
  def createAncestors: Seq[Node] = {
    path.getAncestors.map{
          case ancestorPath: Path => 
            new Object(
              Vector(
                new QlmID(
                  ancestorPath.last
                )
              ),
              ancestorPath
            )
        }.toVector
  }
  def createParent: Node = {
    val parentPath = path.getParent
    if( parentPath.isEmpty || parentPath == Path( "Objects") ){
      new Objects()
    } else {
      new Object(
        Vector(
          QlmID(
            parentPath.last
          )
        ),
        parentPath
      )
    }
  }

  implicit def asObjectType( infoitems: Seq[InfoItemType], objects: Seq[ObjectType] ) : ObjectType = {
    ObjectType(
      /*Seq( QlmID(
        path.last, // require checks (also in OdfObject)
        attributes = Map.empty
      )),*/
      id.map(_.asQlmIDType), //
      descriptions.map( des => des.asDescriptionType ).toSeq,
      infoitems,
      objects,
      attributes = (
       attributesToDataRecord(attributes) ++  typeAttribute.map{ n => ("@type" -> DataRecord(n))}).toMap
    )
  }
    
}
