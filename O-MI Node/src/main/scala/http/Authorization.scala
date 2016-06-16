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

package http

import scala.util.{Failure, Success, Try}

import akka.event.LoggingAdapter
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Directive.SingleValueModifiers
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.util.Tupler._
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import types.OmiTypes._


//////////////////
// (but we couldn't do that in this trait because you can't define abstract private fields
// in traits, neither can you extend any more of these into one class because of the type parameter)
/////////////////

/**
 * This trait defines the base interface for authorization implementations, based on
 * http headers or other http request data. All usages should extend AuthorizationSupportBase
 * before this for the support of combining many authorization implementations.
 *
 * Implementations should extend [[AuthorizationExtension]] trait.
 */
//sealed trait Authorization[UserData] {

  
  /** This directive gets the user identification data from the request.
   */
  //private def extractUserData: Directive1[UserData]

  /** Tests if user specified by [[UserData]] has permission for request [[OmiRequest]].
   * Function is in curried format.
   *
   * @return Boolean, true if connection is permited to do input.
   */
  //private def hasPermission: UserData => OmiRequest => Boolean


  //def userToString: UserData => String = _.toString

//}



object Authorization {
  /**
   * Permission tests return None if request is unauthorized or Some(sameOrNewRequest)
   * if the user is authorized to make the `sameOrNewRequest` that can have some unauthorized
   * objects removed or otherwise limit the original request.
   */
  type PermissionTest = OmiRequest => Option[OmiRequest]

  /** Simple private container for forcing the combination of previous authorization.
   *  Call the apply for the result.
   */
  final class CombinedTest private[Authorization] (test: Directive1[PermissionTest]) {
    def apply(): Directive1[PermissionTest] = test
  }

  /** 
   *  Base trait for authorization in Stackable trait pattern.
   *  (http://www.artima.com/scalazine/articles/stackable_trait_pattern.html)
   *  
   *  Extend this for new authorization plugin traits
   */
  trait AuthorizationExtension {

    /**
     * For easy to access logging in extensions.
     * Will get implemented on the service level anyways.
     */
    def log: Logger = LoggerFactory.getLogger("AuthorizationExtensionDefaultLogger")

    def makePermissionTestFunction: CombinedTest // Directive1[PermissionTest]

    trait Hole
    def myHole(h: Hole) = ???


    private[this] def combineTests(otherTest: PermissionTest, ourTest: PermissionTest): PermissionTest = {
      (request: OmiRequest) =>
        otherTest(request) orElse (Try{ ourTest(request) } // If any authentication method succeeds
          match {  // catch any exceptions, because we want to try other, possibly working extensions too

            case Success(result) => result : Option[OmiRequest]
            case Failure(exception) =>
              log.error("While running authorization extensions, trying next extension", exception)
              None : Option[OmiRequest]
          }
        )
    }

    /** Template for abstract override of makePermissionTestFunction.
     *  Stackable trait pattern; Combines other traits' functionality
     * @param prev Should be always super.makePermissionTestFunction.
     * @param next Should be a new implementation of authorization.
     * @return Combined test
     */
    final protected def combineWithPrevious(
      prev: CombinedTest,
      next: Directive1[PermissionTest]
         ): CombinedTest = 

      new CombinedTest( for {
        otherTest <- prev() :Directive1[PermissionTest]
        ourTest   <- next : Directive1[PermissionTest]

      } yield combineTests(otherTest, ourTest))
  }

  /** 
   *  Core trait for authorization support in Stackable trait pattern.
   *  One of these need to be extended before stackable extension traits.
   *  Doesn't grant any permissions to anyone.
   */
  trait ExtensibleAuthorization extends AuthorizationExtension {
    /**
     * This directive is supposed to extract all required data for any user authorization.
     * The function extracted takes a OmiRequest and returns a Boolean indicating whether
     * the user is a valid user and authorized for the given request.
     *
     * The function can be used many times for the same user.
     *
     * One could make the Authorization implementations splitted in two parts:
     * // This directive gets the user identification data from the request.
     * private def extractUserData: Directive1[UserData]
     *
     * // Tests if user specified by [[UserData]] has permission for request [[OmiRequest]].
     * // Function is in curried format.
     * // @return Boolean, true if connection is permited to do input.
     * private def hasPermission: UserData => OmiRequest => Boolean
     *
     * def makePermissionTestFunction =
     *   combineWithPrevious(super.makePermissionTestFunction, extractUserData map hasPermission)
     *
     * NOTE: Put this as up in routing DSL as possible because some extractors seem to not
     * working properly otherwise.
     */
    def makePermissionTestFunction: CombinedTest = new CombinedTest(
      provide(_ => None)
    )
  }


  /**
   * Template for any authorization implementations. This enables the combination of many
   * authorization methods in the service using [[makePermissionTestFunction]] that combines
   * all [[hasPermission]] functions.
   */
  //trait AuthorizationExtension extends AuthorizationExtSupport {
  //}

}
import http.Authorization._

/** Dummy authorization, allows everything. Can be used for testing, disabling authorization
 *  temporarily and serves as an example of how to extend [[Authorization]] as a Stackable trait.
 */
trait AllowAllAuthorization extends AuthorizationExtension {
  abstract override def makePermissionTestFunction: CombinedTest = 
    combineWithPrevious(super.makePermissionTestFunction, provide(req => Some(req)))
}


/** Allows non [[PermissiveRequest]] to all users (currently read, subs, cancel,
 *  but not write and response). Intended to be used as a last catch-all test (due to logging).
 */
trait AllowNonPermissiveToAll extends AuthorizationExtension {
  abstract override def makePermissionTestFunction: CombinedTest = combineWithPrevious(
    super.makePermissionTestFunction,
    provide{
      case r: PermissiveRequest =>
        None
      case r: OmiRequest =>
        Some(r)
    }
  )
}

/**
 * Intended to be used at the bottom of trait stack to catch all unauthorized
 * requests and log them. Never gives permissions.
 */
trait LogUnauthorized extends AuthorizationExtension {
  private type UserInfo = Option[java.net.InetAddress]
  private def extractIp: Directive1[UserInfo] = extractClientIP map (_.toOption)
  private def logFunc: UserInfo => OmiRequest => Option[OmiRequest] = {ip => {
    case r : OmiRequest =>
      log.warn(s"Unauthorized user from ip $ip: tried to make ${r.getClass.getSimpleName}.")
      None
  }}


  abstract override def makePermissionTestFunction: CombinedTest = combineWithPrevious(
    super.makePermissionTestFunction,
    extractIp map logFunc
  )
}

/**
 * Log the beginning of all permissive requests (e.g. write, response write).
 * Intended to be used at the top of trait stack to catch all requests.
 * Never gives any permissions.
 */
trait LogPermissiveRequestBeginning extends AuthorizationExtension {
  abstract override def makePermissionTestFunction: CombinedTest = combineWithPrevious(
    super.makePermissionTestFunction,
    provide{
      case r: PermissiveRequest with OdfRequest =>
        log.info(s"Permissive request received: ${r.getClass.getSimpleName}: " +
          r.odf.paths.take(3).mkString(", ") + "...")
        None
      case r: PermissiveRequest =>
        log.info(s"Permissive request received: ${r.toString.take(80)}...")
        None
      case _ => None
    }
  )
}
