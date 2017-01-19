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

package analytics

import java.sql.Timestamp
import java.util.{TimeZone, Calendar}

import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import scala.util.Try

import agentSystem.{ResponsibilityRequest, AgentSystem}
import akka.actor.{Props, ActorSystem}
import akka.http.scaladsl.model.RemoteAddress
import akka.stream.ActorMaterializer
import akka.pattern.ask
import akka.testkit.{TestKit, TestActorRef, TestProbe}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import database.{TestDB, OdfTree, SingleStores}
import http.OmiConfigExtension
import org.prevayler.Transaction
import org.specs2.mutable.Specification
import org.specs2.matcher.XmlMatchers._
import org.specs2.mock.Mockito
import org.specs2.specification.AfterAll
import responses.{RequestHandler, SubscriptionManager, CallbackHandler}
import types.{Path, OdfTypes}
import types.OdfTypes._
import types.OmiTypes.{ReadRequest, WriteRequest}

//Very basic test for testing that the analytics results are consistent every patch
class AnalyticsStoreTest extends Specification with Mockito with AfterAll {
  implicit val system = ActorSystem("AnalyticsStoreTest", ConfigFactory.parseString(
    """
            akka.loggers = ["akka.testkit.TestEventListener"]
            akka.stdout-loglevel = INFO
            akka.loglevel = WARNING
            akka.log-dead-letters-during-shutdown = off
            akka.jvm-exit-on-fatal-error = off
    """))
  def afterAll = Await.ready(system.terminate(), 2 seconds)
  import system.dispatcher
  implicit val materializer: ActorMaterializer = ActorMaterializer()(system)
  val conf = ConfigFactory.load("testconfig")
  implicit val settings = new OmiConfigExtension(
    conf
  )
  implicit val callbackHandler: CallbackHandler = new CallbackHandler(settings)( system, materializer)

  implicit val singleStores = new SingleStores(settings)
  implicit val dbConnection: TestDB = new TestDB("subscription-test-db")(
    system,
    singleStores,
    settings
  )

  val subscriptionManager = system.actorOf(SubscriptionManager.props(), "subscription-handler")

  val analyticsStore = system.actorOf(AnalyticsStore.props(singleStores, enableWriteAnalytics = true, enableReadAnalytics = true, enableUserAnalytics = true, 10 minutes, 10 minutes, 10 minutes, 5, 5, 2 seconds))

  val agentSystem = system.actorOf(
    AgentSystem.props(Some(analyticsStore)),
    "agent-system-test"
  )

  val requestHandler : RequestHandler = new RequestHandler(
  )(system,
    agentSystem,
    subscriptionManager,
    settings,
    dbConnection,
    singleStores,
    Some(analyticsStore)
  )

  val calendar = Calendar.getInstance()
  // try to fix bug with travis
  val timeZone = TimeZone.getTimeZone("Etc/GMT+2")
  calendar.setTimeZone(timeZone)
  val date = calendar.getTime
  val testtime = new java.sql.Timestamp(date.getTime)
  def sendRR(user: Int, metadata: Boolean) = {
    val readReq = {
      val p: Path = Path("Objects/AnalyticsStoreTest/first")
      val _odf =
        if(!metadata)OdfTypes.createAncestors(OdfInfoItem(p))
        else OdfTypes.createAncestors(OdfInfoItem(p, metaData = Some(OdfMetaData(OdfTreeCollection.empty))))
      ReadRequest(
        odf = _odf,
        user = Some(RemoteAddress.apply(bytes = Array[Byte](127,0,0,user + 1 toByte)))
      )
    }
      requestHandler.handleRead(readReq)

  }
  addValue("first", Vector(OdfValue("1", new Timestamp(testtime.getTime-4000))))
  addValue("first", Vector(OdfValue("2", new Timestamp(testtime.getTime-3000))))
  addValue("first", Vector(OdfValue("3", new Timestamp(testtime.getTime-2000))))
  addValue("first", Vector(OdfValue("4", new Timestamp(testtime.getTime-1000))))
  addValue("first", Vector(OdfValue("5", new Timestamp(testtime.getTime))))
  Await.ready(sendRR(1, metadata = false), 2 seconds)
  Thread.sleep(100)
  Await.ready(sendRR(1, metadata = false), 2 seconds)
  Thread.sleep(200)
  Await.ready(sendRR(2, metadata = false), 2 seconds)
  Thread.sleep(300)
  Await.ready(sendRR(2, metadata = false), 2 seconds)
  Thread.sleep(400)
  Await.ready(sendRR(3, metadata = false), 2 seconds)
  def addValue(path: String, nv: Vector[OdfValue[Any]]): Unit = {
    val pp = Path("Objects/AnalyticsStoreTest/")
    val odf = OdfTypes.createAncestors(OdfInfoItem(pp / path, nv))
    val writeReq = WriteRequest( odf)
    implicit val timeout = Timeout( 10 seconds )
    val future = requestHandler.handleWrite(writeReq)
    Await.ready(future, 10 seconds)// InputPusher.handlePathValuePairs(Seq((pp / path, nv)))
  }
  sequential
  "Analytics Store" should {
    "return correct analytical values after defined time" in {
      Thread.sleep(3000)
      val res = Await.result(sendRR(0, metadata = true).map(_.asXML), 2 seconds)
      println(res)
      val infoItems = res \\("InfoItem") \("MetaData") \("InfoItem")
      infoItems.foreach(ii => println(ii.attributes.asAttrMap))
      val uniqueUsers = infoItems.find(_.\@("name") =="uniqueUsers").flatMap(_.\("value").headOption).map(_.text)//.map(_.toString.toInt)
      val numAccess = infoItems.find(_.\@("name") == "NumAccess").flatMap(_.\("value").headOption).map(_.text)
      val popularity = infoItems.find(_.\@("name") == "popularity").flatMap(_.\("value").headOption).flatMap(n => Try(n.text.toDouble).toOption)//.toDouble)
      val freshness = infoItems.find(_.\@("name") == "freshness").flatMap(_.\("value").headOption).flatMap(n=> Try(n.text.toDouble).toOption)//.toDouble)
      val numWrites = infoItems.find(_.\@("name") == "NumWrites").flatMap(_.\("value").headOption).map(_.text)
      "for unique users" in{
        uniqueUsers must beSome(beEqualTo("3"))
      }
      "for number of accesses" in{
        numAccess must beSome(beEqualTo("5"))
      }
      "for popularity" in{
        popularity must beSome(beCloseTo(0.2, 0.09))
      }
      "for freshness" in{
        freshness must beSome(beCloseTo(1.0, 0.09))
      }
      "for number of writes" in{
        numWrites must beSome(beEqualTo("5"))
      }
      //res must \\("Object") \("InfoItem") \ ("MetaData") \ ("InfoItem","name" ->"uniqueUsers") \ ("value") \> ("3")
    }
  }
}