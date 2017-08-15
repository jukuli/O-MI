
package agents
package parkingService

import scala.math
import types.OdfTypes._
import types._

sealed trait ParkingEvent
case class Reservation( path: Path, user: String, openLid: Boolean = false )extends ParkingEvent{

  def toOdf: OdfObject ={
    val lidStatus = if( openLid ){
      Vector(
      OdfObject(
        Vector( QlmID( "Charger" )),
        path / "Charger",
        infoItems = Vector(
          OdfInfoItem(
            path / "Charger" / "LidStatus",
            values = Vector( OdfValue( "Open", currentTime ) )
          )
        )
      ))
    } else Vector()
    OdfObject(
      Vector( QlmID( path.last )),
      path,
      objects = lidStatus,
      infoItems = Vector(
        OdfInfoItem(
          path / "Available",
          values = Vector( OdfValue( "false", currentTime ) )
        ),
        OdfInfoItem(
          path / "User",
          values = Vector( OdfValue( user, currentTime ) )
        )
      )
    )
  }
}
case class FreeReservation( path: Path, user: String, openLid: Boolean = false )extends ParkingEvent{

  def toOdf: OdfObject ={
    val lidStatus = if( openLid ){
      Vector(
      OdfObject(
        Vector( QlmID( "Charger" )),
        path / "Charger",
        infoItems = Vector(
          OdfInfoItem(
            path / "Charger" / "LidStatus",
            values = Vector( OdfValue( "Open", currentTime ) )
          )
        )
      ))
    } else Vector()
    OdfObject(
      Vector( QlmID( path.last )),
      path,
      objects = lidStatus,
      infoItems = Vector(
        OdfInfoItem(
          path / "Available",
          values = Vector( OdfValue( "true", currentTime ) )
        ),
        OdfInfoItem(
          path / "User",
          values = Vector( OdfValue( "NONE", currentTime ) )
        )
      )
    )
  }
}
case class OpenLid( path: Path, user: String ) extends ParkingEvent{

  def lidStatusPath = path / "Charger" / "LidStatus"
  def toOdf: OdfObject ={
    OdfObject(
      Vector( QlmID( path.last )),
      path,
      objects = Vector(
        OdfObject(
          Vector( QlmID( "Charger" )),
          path / "Charger",
          infoItems = Vector(
            OdfInfoItem(
              path / "Charger" / "LidStatus",
              values = Vector( OdfValue( "Open", currentTime ) )
            )
          )
        )
      )
    )
  }
}
