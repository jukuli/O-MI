
package types.odf;

import java.lang.Object;
import java.util.Vector;
import java.util.Map;
import scala.Option;
import scala.collection.JavaConverters;
import java.sql.Timestamp;
import scala.collection.immutable.HashMap;
import scala.collection.immutable.HashMap$;
import types.odf.*;
import types.odf.Value;

  /**
   * Factory class for creating O-DF types used in Scala.
   */
public class ODFFactory{

  /**
   *
   * @param value Value inside of O-DF value element.
   * @param typeValue Type of value, one of built in XML Schema data types specifed in
   *  <a href="https://www.w3.org/TR/xmlschema-2/#built-in-datatypes">XML Schema types</a>
   *  Parameter value is cast to type specifed by typeValue parameter. If cast fails, value's
   *  type will be String.
   * @param timestamp Timestamp when value was measured or received.
   * @param attributes Map containing proprietary attributes for value.
   * @return Value
  */
  public static Value<Object> createValue(
    String value,
    String typeValue,
    Timestamp timestamp,
    Map<String,String> attributes
  ){
    return Value$.MODULE$.applyFromString(
        value,
        typeValue,
        timestamp,
        HashMap$.MODULE$.<String,String>apply(
          JavaConverters.mapAsScalaMapConverter(attributes).asScala().toSeq()
          )
    );
  }
  public static Description createDescription(
    String text,
    String language
  ){
    return new Description(
        text,
        Option.apply( language ) 
    );
  }
  public static QlmID createQlmID(
      String id,
      String idType,
      String tagType,
      Timestamp startDate,
      Timestamp endDate,
      Map<String,String> attributes
  ){

    return new QlmID(
        id,
        Option.apply(idType),
        Option.apply(tagType),
        Option.apply(startDate),
        Option.apply(endDate),
        HashMap$.MODULE$.<String,String>apply(
          JavaConverters.mapAsScalaMapConverter(attributes).asScala().toSeq()
          )
    );
  }
  public static InfoItem createInfoItem( 
      String nameAttribute,
      Path path,
      String typeAttribute,
      Iterable<QlmID> names,
      Iterable<Description> descriptions,
      Iterable<Value<Object>> values,
      MetaData metaData,
      Map<String,String> attributes
  ){
    return new InfoItem(
        nameAttribute,
        path,
        Option.apply(typeAttribute),
        JavaConverters.iterableAsScalaIterableConverter(names).asScala().toVector(),
        JavaConverters.iterableAsScalaIterableConverter(descriptions).asScala().toVector(),
        JavaConverters.iterableAsScalaIterableConverter(values).asScala().toVector(),
        Option.apply(metaData),
        HashMap$.MODULE$.<String,String>apply(
          JavaConverters.mapAsScalaMapConverter(attributes).asScala().toSeq()
          )
    );
  }
  public static MetaData createMetaData(
    Iterable<InfoItem> infoItems
  ){

    return new MetaData(
      JavaConverters.iterableAsScalaIterableConverter(infoItems).asScala().toVector()
    );
  }

  public static types.odf.Object createObject( 
      Iterable<QlmID> ids,
      Path path,
      String typeAttribute,
      Iterable<Description> descriptions,
      Map<String,String> attributes
  ){
    return new types.odf.Object(
        JavaConverters.iterableAsScalaIterableConverter(ids).asScala().toVector(),
        path,
        Option.apply(typeAttribute),
        JavaConverters.iterableAsScalaIterableConverter(descriptions).asScala().toVector(),
        HashMap$.MODULE$.<String,String>apply(
          JavaConverters.mapAsScalaMapConverter(attributes).asScala().toSeq()
          )
    );
  }
      
  public static Objects createObjects( 
      String version,
      Map<String,String> attributes
  ){
    return new Objects(
        Option.apply(version),
        HashMap$.MODULE$.<String,String>apply(
          JavaConverters.mapAsScalaMapConverter(attributes).asScala().toSeq()
          )
    );
  }

  public static ImmutableODF createImmutableODF(
    Iterable<Node> nodes
  ){
    return ImmutableODF.apply(
      JavaConverters.iterableAsScalaIterableConverter(nodes).asScala().toVector()
    );
  }
  public static MutableODF createMutableODF(
    Iterable<Node> nodes
  ){
    return MutableODF.apply( 
      JavaConverters.iterableAsScalaIterableConverter(nodes).asScala().toVector()
    );
  }

}
