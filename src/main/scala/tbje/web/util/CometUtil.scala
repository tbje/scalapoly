package tbje.web.util

import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.http.S
import net.liftweb.http.{ S => LS, _ }
import net.liftweb.http.js.JsCmds.Noop

object CometUtil {
  def get[T](implicit manifest: Manifest[T]): Box[LiftCometActor] = {
    S.session.flatMap { session =>
      Box(session.findComet(manifest.runtimeClass.getSimpleName).headOption)
    }
  }

  def sendMessage[T <: CometActor, B](c: Class[T], msg: B) = {
    LS.session.map(_.sendCometActorMessage(c.getName().replace("tbje.web.comet.", ""), None, msg))
    Noop
  }

}
