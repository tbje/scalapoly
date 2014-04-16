package tbje.web.comet

import scala.xml.NodeSeq
import net.liftweb.http.{ S, CometActor }
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmds.Noop
import net.liftweb.http.js.JsCmds.Alert
import net.liftweb.util.Schedule
import akka.actor.Actor
import net.liftweb.json.JsonAST
import net.liftweb.json.JsonAST._
import net.liftweb.http.js.JsCmd
import scala.collection.mutable.Map
import net.liftweb.util.JsonCommand
import akka.actor.Props
import tbje.web.util.Config
import tbje.web.util.HtmlUtil._
import tbje.web.util.JsUtil._
import tbje.web.util.UniqueRandomKey
import net.liftweb.http.js.JsCmds.JsTry
import net.liftweb.http.js.JsCmds.JsCrVar

class JsConsoleComet extends CometActor {
  override def render = NodeSeq.Empty

  JsConsoleActor() ! JsConsoleActor.messages.Reg(this)

  override val autoIncludeJsonCode = true

  override def lowPriority = {
    case (code: String, js: String) =>
      val res = "res345345"
      val send = jsonSend("reply", js"{'code': '$code', 'res': $res}").toJsCmd
      val assign = JsCrVar(res, js"$js")
      val tryCatch = js"try{ $send } catch (e) { var $res = e.toString(); $send; }"
      partialUpdate(assign & tryCatch)
    case x =>
      println("did not understand: " + x)
      partialUpdate(Alert("Did not understand: " + x))
  }
  import JsonCommand.iterableToOption
  import net.liftweb.json.JsonAST._
  import net.liftweb.common._
  override def receiveJson: PartialFunction[JsonAST.JValue, JsCmd] = {
    case JsonCommand("reply", _, params) =>
      for {
        code <- (params \ "code").toOpt
        JString(x) <- code.filter(_.isInstanceOf[JString]).map(_.asInstanceOf[JString])
        res = (params \ "res").toOpt
      } {
        JsConsoleActor() ! JsConsoleActor.messages.Returned(x, res.map(_.values.toString).getOrElse("()"))
      }
      Noop
    case other =>
      println("Got something else" + other)
      Noop
  }
}

class JsConsoleActor extends Actor {
  import JsConsoleActor.messages._
  import scala.collection.mutable.Buffer

  val comets: Buffer[JsConsoleComet] = Buffer.empty
  val call: Map[String, String => Unit] = Map.empty

  def receive = {
    case Reg(comet) =>
      comets.append(comet)
    case Msg(code, js) =>
      comets.foreach { _ ! (code, js) }
    case Return(code, promise) =>
      call(code) = promise
    case Returned(code, res) =>
      println(s"returned: $code ${call.get(code).isDefined} - $res")
      call(code)(res)
      call - code
    case GetCode =>
      UniqueRandomKey { key =>
        !call.isDefinedAt(key)
      } foreach { key =>
        sender ! key
      }
  }
}
object JsConsoleActor {
  private val zeeActor = Config.system.actorOf(Props[JsConsoleActor])
  def apply() = zeeActor
  object messages {
    case class Reg(ca: JsConsoleComet)
    case class Msg(code: String, js: String)
    case class Return(code: String, promise: String => Unit)
    case class Returned(code: String, res: String)
    case object GetCode
  }
}
