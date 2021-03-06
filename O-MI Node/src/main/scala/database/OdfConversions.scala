/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 +    Copyright (c) 2015 Aalto University.                                        +
 +                                                                                +
 +    Licensed under the 4-clause BSD (the "License");                            +
 +    you may not use this file except in compliance with the License.            +
 +    You may obtain a copy of the License at top most directory of project.      +
 +                                                                                +
 +    Unless required by applicable law or agreed to in writing, software         +
 +    distributed under the License is distributed on an "AS IS" BASIS,           +
 +    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    +
 +    See the License for the specific language governing permissions and         +
 +    limitations under the License.                                              +
 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
package database

import scala.language.postfixOps
//import scala.collection.JavaConversions.iterableAsScalaIterable
import scala.collection.{SortedMap, breakOut}

import types.OdfTypes.OdfTreeCollection.seqToOdfTreeCollection
import types.OdfTypes._

trait OdfConversions extends OmiNodeTables {
  type DBValueTuple = (DBNode, Option[DBValue])
  type DBInfoItem = (DBNode, Seq[DBValue])
  type DBInfoItems = SortedMap[DBNode, Seq[DBValue]]

  def toDBInfoItems(input: Seq[DBValueTuple]): DBInfoItems = {
    SortedMap {
      input.groupBy(_._1).map {
        case (node, valuetuple) =>

          val dbValues = valuetuple.collect {
            case (_, Some(dbValue)) => dbValue
          }.reverse //maybe foldLeft so reverse is not needed?

          (node, dbValues)
      }(breakOut): _*
    }(DBNodeOrdering)
  }
  def toDBInfoItem(tupleData: Seq[DBValueTuple]): Option[DBInfoItem] = {
    val items = toDBInfoItems(tupleData)
    assert(items.size <= 1, "Asked one infoitem, should contain max one infoitem")
    items.headOption
  }

  /**
   * Conversion for a (sub)tree of hierarchy with value data.
   * @param treeData Hierarchy and value data joined, so contains InfoItem DBNodes and its values.
   */
  protected[this] def odfConversion(treeData: Seq[DBValueTuple]): Option[OdfObjects] = {
    // Convert: Map DBNode -> Seq[DBValue]
    val nodeMap = toDBInfoItems(treeData)
    odfConversion(nodeMap)
  }

  protected[this] def hasPathConversion: DBInfoItem => OdfNode = {
    case (infoItemNode, values) if infoItemNode.isInfoItem =>
      val odfValues = values map (_.toOdf) toIterable
      val odfInfoItem = infoItemNode.toOdfInfoItem(odfValues)
      odfInfoItem
    case (objectNode, values) if !objectNode.isInfoItem && objectNode.depth > 1 =>
      objectNode.toOdfObject
    case (objectNode, values) if !objectNode.isInfoItem && objectNode.depth == 1 =>
      objectNode.toOdfObjects
    case matchError => throw new MatchError(matchError)
  }

  /**
   * For url discovery get path.
   * Version for which Object children are supported for one level of the hierarchy.
   * @param items input data for single object (objects and infoitems for the first level of children)
   * @return Single object or infoItem extracted from items
   */
  protected[this] def singleObjectConversion(
    items: DBInfoItems
  ): Option[OdfNode] = items.size match {
      case 0 => None

      case pos if pos > 0 =>
    val nodes = items.keys

    // search element with the lowest depth and take only the node
    val theObject = items minBy (_._1.depth)

    theObject match {
      case (objectNode, _) if !objectNode.isInfoItem =>
        val allChildren =
          nodes filter (item =>
            item.leftBoundary > objectNode.leftBoundary &&
              item.leftBoundary < objectNode.rightBoundary)

        val (infoItemChildren, objectChildren) = allChildren partition (_.isInfoItem)

        val odfInfoItemChildren = infoItemChildren map (_.toOdfInfoItem)
        val odfObjectChildren = objectChildren map (_.toOdfObject)

        require(allChildren.nonEmpty, s"should have children, has $items")

        if (objectNode.depth == 1) {
          val odfObjects = OdfObjects(odfObjectChildren)
          Some(odfObjects)
        } else {
          val odfObject = objectNode.toOdfObject(odfInfoItemChildren, odfObjectChildren)
          Some(odfObject)
        }

      case infoItem @ (infoItemNode, _) if infoItemNode.isInfoItem =>
        Some(hasPathConversion(infoItem))

      case matchError => throw new MatchError(matchError)
    }
  
      case default: Int => 
      None
  }

  protected[this] def odfConversion: DBInfoItem => OdfObjects =
    createAncestors _ compose hasPathConversion

  protected[this] def odfConversion(treeData: DBInfoItems): Option[OdfObjects] = {
    val odfObjectsTrees = treeData map odfConversion

    // safe version of reduce
    odfObjectsTrees.headOption map { head =>
      odfObjectsTrees.par.reduce(_ union _)
    }
  }
}
