package tbje.web.util

import org.slf4j.LoggerFactory

import net.liftweb.http.S
import net.liftweb.common.{ Logger => LiftLogger }
import _root_.org.slf4j.Marker
import net.liftweb.util.ThreadGlobal

object ThreadUserName extends ThreadGlobal[Option[String]] {
  this.set(None)
}

trait Logger {
  object logger extends LiftLogger {
    def addContext(msg: => AnyRef) = {
      List(
        //UserSession.is.user.map(_.username).orElse(ThreadUserName.box.flatMap(x => x)).getOrElse(" - "),
        S.session.map(_.uniqueId).getOrElse(" - "),
        String.valueOf(msg)).mkString(" | ")
    }

    override def assertLog(assertion: Boolean, msg: => String) = if (assertion) info(msg)

    override def trace[T](msg: String, v: T): T = { super.trace(addContext(msg), v) }
    override def trace(msg: => AnyRef) = super.trace(addContext(msg))
    override def trace(msg: => AnyRef, t: Throwable) = super.trace(addContext(msg).asInstanceOf[Function0[AnyRef]], t)
    override def trace(msg: => AnyRef, marker: Marker) = super.trace(addContext(msg, marker))
    override def trace(msg: => AnyRef, t: Throwable, marker: => Marker) = super.trace(addContext(msg, t, marker))

    override def debug(msg: => AnyRef) = super.debug(addContext(msg))
    override def debug(msg: => AnyRef, t: Throwable) = super.debug(addContext(msg), t)
    override def debug(msg: => AnyRef, marker: Marker) = super.debug(addContext(msg), marker)
    override def debug(msg: => AnyRef, t: Throwable, marker: Marker) = super.debug(addContext(msg), t, marker)

    override def info(msg: => AnyRef) = super.info(addContext(msg))
    //override def info(msg: => AnyRef, t:  Throwable) = super.info(addContext(msg), t)
    override def info(msg: => AnyRef, marker: Marker) = super.info(addContext(msg), marker)
    override def info(msg: => AnyRef, t: Throwable, marker: Marker) = super.info(addContext(msg), t, marker)

    override def warn(msg: => AnyRef) = super.warn(addContext(msg))
    override def warn(msg: => AnyRef, t: Throwable) = super.warn(addContext(msg), t)
    override def warn(msg: => AnyRef, marker: Marker) = super.warn(addContext(msg), marker)
    override def warn(msg: => AnyRef, t: Throwable, marker: Marker) = super.warn(addContext(msg), t, marker)

    override def error(msg: => AnyRef) = super.error(addContext(msg))
    override def error(msg: => AnyRef, t: Throwable) = super.error(addContext(msg), t)
    override def error(msg: => AnyRef, marker: Marker) = super.error(addContext(msg), marker)
    override def error(msg: => AnyRef, t: Throwable, marker: Marker) = super.error(addContext(msg), t, marker)
  }
}
