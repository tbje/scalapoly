package tbje.web.util

import net.liftweb.common.Box
import net.liftweb.common.Full
import net.liftweb.common.Empty
import net.liftweb.http.rest.RestContinuation
import net.liftweb.http.Req
import net.liftweb.http.LiftRules
import net.liftweb.http.PostRequest
import net.liftweb.http.S
import net.liftweb.http.JsonResponse
import net.liftweb.json.JsonDSL._
import tbje.web.comet.JsConsoleComet
import tbje.web.comet.JsConsoleActor
import akka.pattern.ask
import akka.util.Timeout

object JsConsoleWs extends Logger {

  def matcher: LiftRules.DispatchPF = {
    case r @ Req("js" :: Nil, _, PostRequest) if r.remoteAddr == "127.0.0.1" =>
      val is = r.rawInputStream
      import JsConsoleActor.messages._
      try {
        implicit val timeout: Timeout = Timeout(13000)
        import scala.concurrent.ExecutionContext.Implicits.global
        val code = (JsConsoleActor() ? GetCode).mapTo[String]
        val cmd = is.map(is => io.Source.fromInputStream(is).getLines.mkString(" ")).getOrElse("""alert("no input")""")
        println(cmd)
        RestContinuation.async { satisfyRequest =>
          code.onSuccess {
            case code =>
              JsConsoleActor() ! Msg(code, cmd)
              JsConsoleActor() ! Return(code, (x: String) => {
                println(s"should satisfy with $x")
                satisfyRequest(JsonResponse("result" -> x))
              })
          }
        }
      } finally {
        is.map(_.close())
      }
  }
}

object JsConsole {

  import dispatch.{ Req => DReq, _ }
  import dispatch.Http._
  import scala.concurrent.duration._
  import scala.concurrent.Await
  import net.liftweb.http.js.{ JsExp, JsCmd, JsMember }
  implicit class JsExpOpt(j: JsExp) { def e = ev(j.toJsCmd) }
  implicit class JsCmdOpt(j: JsCmd) { def e = ev(j) }
  implicit class JsMemberOpt(j: JsMember) { def e = ev(j.toJsCmd) }

  val http = new Http

  def ev(query: String) =
    restCall(url << query)

  def ev(query: JsCmd) =
    restCall(url << query.toJsCmd)

  val url = host("localhost", 8080) / "js"

  def restCall(req: DReq) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val f: scala.concurrent.Future[Either[String, String]] = http(req > (_ match {
      case r if r.getStatusCode == 200 =>
        Right(r.getResponseBody)
      case r if r.getStatusCode == 401 =>
        Left(s"FORBIDDEN: while accessing coinbase got 401 with the content: ${r.getResponseBody}")
      case r =>
        Left(s"While accessing coinbase got status ${r.getStatusCode} with the content: ${r.getResponseBody}")
    }))
    val res = scala.util.control.Exception.allCatch either Await.result(f, 3.seconds)
    res.fold(
      failure => Left(failure.getMessage),
      ok => ok)
  }

  def shutdown() = http.shutdown()

}

