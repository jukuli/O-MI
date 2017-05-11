package types
package odf

import scala.collection.immutable.HashMap
import types.OdfTypes._
import types.OdfTypes.{QlmID => OdfQlmID }

object NewTypeConverter{
  def convertODF[M <: scala.collection.Map[Path,Node], S <: scala.collection.SortedSet[Path]](
    o_df: ODF[M,S] 
  ) : OdfObjects ={
    val firstLevelObjects= o_df.getChilds( new Path("Objects") )
    val odfObjects= firstLevelObjects.map{
      case obj: Object => 
        createOdfObject( obj, o_df )
    }
    o_df.nodes.get(new Path("Objects")).collect{
      case objs: Objects =>
        convertObjects(objs,odfObjects) 
    }.getOrElse{
      OdfObjects()
    }
  }

  def createOdfObject[M <: scala.collection.Map[Path,Node], S <: scala.collection.SortedSet[Path]]
  ( obj: Object, o_df: ODF[M,S] ): OdfObject ={
    val (objects, infoItems ) = o_df.getChilds( obj.path ).partition{
      case obj: Object => true
      case ii: InfoItem => false
    }
    convertObject(
      obj,
      infoItems.collect{
        case ii: InfoItem => convertInfoItem(ii) 
      },
      objects.collect{
        case obj: Object =>
          createOdfObject( obj, o_df ) 
      }
    )
  }
  def convertObjects(
    objs: Objects,
    objects: Seq[OdfObject]
  ) : OdfObjects ={
    OdfObjects(
      objects.toVector,
      objs.version
    )
  }
  def convertObject( obj: Object, 
    infoItems: Seq[OdfInfoItem] = Vector.empty,
    objects: Seq[OdfObject] = Vector.empty
  ) : OdfObject = {
    var ids = obj.id.map( convertQlmID(_)).toVector 
    if( !ids.contains(obj.path.last ) ){
      ids = ids ++ Vector( OdfQlmID(obj.path.last) )
    }
    OdfObject(
      ids,
      types.Path(obj.path.toSeq),
      infoItems.toVector,
      objects.toVector,
      obj.description.map( convertDescription ).headOption,
      obj.typeAttribute
    )
  } 

  def convertQlmID( id: QlmID ) : OdfQlmID = {
    OdfQlmID(
      id.id,
      id.idType,
      id.tagType,
      id.startDate,
      id.endDate,
      HashMap( id.attributes.toSeq:_* )
    )
  }
  def convertDescription( des: Description ): OdfDescription = {
    OdfDescription( des.text, des.language )
  }
  def convertInfoItem( ii: InfoItem ): OdfInfoItem ={
    OdfInfoItem(
      types.Path( ii.path.toSeq ),
      ii.value.map( convertValue ).toVector,
      ii.description.map( convertDescription ).headOption,
      ii.metaData.map( convertMetaData )
    )
  }
  def convertValue( value: Value[Any] ): OdfValue[Any] = {
    value.value match {
      case o: ODF[scala.collection.Map[Path,Node],scala.collection.SortedSet[Path] ] =>
        OdfValue( convertODF(o), value.timestamp, HashMap( value.attributes.toSeq:_*) )
      case o: Any =>
        OdfValue(o, value.timestamp, HashMap( value.attributes.toSeq:_*) )
    }
  }

  def convertMetaData( md: MetaData ): OdfMetaData = {
    OdfMetaData( md.infoItems.map( ii => convertInfoItem( ii )).toVector )
  }
}
