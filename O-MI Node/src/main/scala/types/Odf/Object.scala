package types
package odf

import scala.collection.{ Seq, Map }
import scala.collection.immutable.HashMap 

import parsing.xmlGen.scalaxb.DataRecord
import parsing.xmlGen.xmlTypes.{InfoItemType, ObjectType}

class Object(
  val id: Seq[QlmID],
  val path: Path,
  val typeAttribute: Option[String] = None,
  val description: Seq[Description] = Vector.empty,
  val attributes: Map[String,String] = HashMap.empty
) extends Node with Unionable[Object] {
  assert( id.nonEmpty )
  def union( that: Object ): Object ={
    val pathsMatches = path == that.path 
    val containSameId = id.map( _.id ).toSet.intersect( that.id.map( _.id).toSet ).nonEmpty
    assert( containSameId && pathsMatches)
    new Object(
      id ++ that.id,//TODO: Some kind unioning needed. If only  one has attributes and other doesn,t, we should compain them to one.
      path,
      (typeAttribute, that.typeAttribute) match {
        case (Some( t ), Some( ot ) ) =>
          if ( t == ot) Some( t ) 
          else Some( t + " " + ot)
        case (t, ot) => t.orElse(ot)
      },
      description ++ that.description,
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

  implicit def asObjectType( infoitems: Seq[InfoItemType], objects: Seq[ObjectType] ) : ObjectType = {
    ObjectType(
      /*Seq( QlmID(
        path.last, // require checks (also in OdfObject)
        attributes = Map.empty
      )),*/
      id.map(_.asQlmIDType), //
      description.map( des => des.asDescriptionType ).toSeq,
      infoitems,
      objects,
      attributes = (attributes.map{
        case ( key, value ) => s"@$key" -> DataRecord(value) 
      }++  typeAttribute.map{ n => ("@type" -> DataRecord(n))}).toMap
    )
  }
  implicit def asOdfObject( 
    infoItems: Seq[types.OdfTypes.OdfInfoItem] = Vector.empty,
    objects: Seq[types.OdfTypes.OdfObject] = Vector.empty
  ) : types.OdfTypes.OdfObject = {
  
    types.OdfTypes.OdfObject(
      id.map( _.asOdfQlmID).toVector ,
      types.Path(path.toSeq),
      infoItems.toVector,
      objects.toVector,
      description.map( _.asOdfDescription ).headOption,
      typeAttribute
    )
    
  } 
    
}
