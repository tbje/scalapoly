package tbje.scalapoly

import spray.json._

object ScalapolyJsonProtocol extends DefaultJsonProtocol {
  import DefaultJsonProtocol._
  implicit object SquareJsonFormat extends RootJsonFormat[Square] {
    val IntReq = """(\d*)""".r
    def write(c: Square) =
      JsString(Game.board.indexOf(c).toString)

    def read(value: JsValue) = value match {
      case JsString(IntReq(squareId)) =>
        Game.board(squareId.toInt)
      case _ => deserializationError("Color expected")
    }
  }
  implicit val f1 = jsonFormat7(Player)
  implicit object GameJsonFormat extends RootJsonFormat[Game] {
    def write(g: Game) =
      JsObject(
        "players" -> g.players.toJson,
        "turn" -> JsNumber(g.turn),
        "doubleCount" -> JsNumber(g.doubleCount))

    def read(value: JsValue) = value.asJsObject.getFields("players", "turn", "doubleCount") match {
      case Seq(JsArray(a), JsNumber(turn), JsNumber(doubleCount)) =>
        Game(a.map(x => x.convertTo[Player]), turn.toInt, doubleCount.toInt)
      case _ => deserializationError("Color expected")
    }
  }
}
