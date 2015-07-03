// Generated by <a href="http://scalaxb.org/">scalaxb</a>.

package parsing
package xmlGen
package xmlTypes




case class OmiEnvelope(omienvelopeoption: scalaxb.DataRecord[OmiEnvelopeOption],
  version: String,
  ttl: Double)

trait OmiEnvelopeOption
trait TargetType

object TargetType {
  def fromString(value: String, scope: scala.xml.NamespaceBinding): TargetType = value match {
    case "device" => Device
    case "node" => Node

  }
}

case object Device extends TargetType { override def toString = "device" }
case object Node extends TargetType { override def toString = "node" }


/** Base type for "read" and "write" requests.
*/
trait RequestBaseTypable {
  val nodeList: Option[NodesType]
  val requestID: Seq[IdType]
  val msg: Option[scalaxb.DataRecord[Any]]
  val callback: Option[java.net.URI]
  val msgformat: Option[String]
  val targetType: TargetType}


/** Base type for "read" and "write" requests.
*/
case class RequestBaseType(nodeList: Option[NodesType] = None,
  requestID: Seq[IdType] = Nil,
  msg: Option[scalaxb.DataRecord[Any]] = None,
  callback: Option[java.net.URI] = None,
  msgformat: Option[String] = None,
  targetType: TargetType) extends RequestBaseTypable


/** Read request type.
*/
case class ReadRequest(nodeList: Option[NodesType] = None,
  requestID: Seq[IdType] = Nil,
  msg: Option[scalaxb.DataRecord[Any]] = None,
  callback: Option[java.net.URI] = None,
  msgformat: Option[String] = None,
  targetType: TargetType = Node,
  interval: Option[Double] = None,
  oldest: Option[Int] = None,
  begin: Option[javax.xml.datatype.XMLGregorianCalendar] = None,
  end: Option[javax.xml.datatype.XMLGregorianCalendar] = None,
  newest: Option[Int] = None) extends RequestBaseTypable with OmiEnvelopeOption


/** Write request type.
*/
case class WriteRequest(nodeList: Option[NodesType] = None,
  requestID: Seq[IdType] = Nil,
  msg: Option[scalaxb.DataRecord[Any]] = None,
  callback: Option[java.net.URI] = None,
  msgformat: Option[String] = None,
  targetType: TargetType = Node
) extends RequestBaseTypable with OmiEnvelopeOption


/** List of results.
*/
case class ResponseListType(result: RequestResultType*) extends OmiEnvelopeOption

trait TargetTypeType

object TargetTypeType {
  def fromString(value: String, scope: scala.xml.NamespaceBinding): TargetTypeType = value match {
    case "device" => DeviceValue
    case "node" => NodeValue

  }
}

case object DeviceValue extends TargetTypeType { override def toString = "device" }
case object NodeValue extends TargetTypeType { override def toString = "node" }


/** Result of a request.
*/
case class RequestResultType(
  returnValue: ReturnType,
  requestID: Option[IdType] = None,
  msg: Option[scalaxb.DataRecord[Any]] = None,
  nodeList: Option[NodesType] = None,
  omiEnvelope: Option[OmiEnvelope] = None,
  msgformat: Option[String] = None,
  targetType: TargetTypeType = NodeValue
)


/** Return status of request. Use HTTP codes / descriptions when applicable.
*/
case class ReturnType(
  value: String,
  returnCode: String,
  description: Option[String] = None,
  attributes: Map[String, scalaxb.DataRecord[Any]])


/** The nodesType is used anywhere in the schema where lists of nodes can appear. 
*/
case class NodesType(node: Seq[java.net.URI] = Nil,
  typeValue: Option[String] = None)


/** Some kind of identifier with optional "format" attribute for indicating what kind of identifier is used. 
*/
case class IdType(value: String,
  format: Option[String] = None)


/** Cancel request type.
*/
case class CancelRequest(nodeList: Option[NodesType] = None,
  requestID: Seq[IdType] = Nil) extends OmiEnvelopeOption

