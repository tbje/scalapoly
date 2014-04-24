package tbje.scalapoly

import tbje.facelift.imports._
import tbje.facelift.css.CssMeasure
import tbje.web.util.HtmlUtil._
import tbje.web.util.JsUtil._
import tbje.facelift.css.CssDeclaration
import scala.xml.NodeSeq
import tbje.web.util.JsConsole._
import tbje.web.templates.Ids._
import tbje.web.util.Config

trait SquareDecorator {
  def name: String
  override def toString = name
  def houses: Int
  def color: String
  def ownerColor: String
  def visitors: Seq[(String, String, String)]
}

object WebGame extends App {

  contentDivId.css(PaddingTop(100.px), Color.hex("#5E5E5E")).e

  Game.init(WebUI.redraw)
  Config.system.shutdown()
  Config.system.awaitTermination
}

object WebUI {
  val availablePlayerColors = Seq("green" -> "white", "yellow" -> "black", "blue" -> "white", "red" -> "white")
  val border = "1px solid black"

  def styleColor(color: String, index: Int, o: Orientations) = {
    val first = color match {
      case "white" => Seq(BackgroundColor.White)
      case color => Seq(BackgroundColor(color), o.outerEdge("1px solid black"))
    }
    val snd = index match {
      case 1 => Seq(o.backwardsEdge("1px solid black"))
      case 5 => Seq(o.forwardsEdge("1px solid black"))
      case _ => Seq()
    }
    val trd = Seq(o.height(6.px), o.width(90 / 5 px), o.boardEdge("1px solid black"))
    Style((first ++ snd ++ trd): _*)
  }

  def styleHouses(houses: Int, index: Int, color: String, o: Orientations) = {
    val fst = houses match {
      case 0 => Seq(o.boardEdge("2px solid " + color), BackgroundColor.White)
      case -1 => Seq(o.boardEdge("2px solid " + color), BackgroundColor.Gray)
      case -2 => Seq(BackgroundColor.White)
      case n if n >= index => Seq(BackgroundColor(color), o.boardEdge("2px solid " + color))
      case n if n > 0 => Seq(o.boardEdge("2px solid " + color))
      case _ => Seq()
    }
    val snd = index match {
      case 1 => Seq(o.backwardsEdge(border))
      case 5 => Seq(o.forwardsEdge(border))
      case n => Seq()
    }
    val all = Seq(o.outerEdge(border), o.height(5.px))
    Style((fst ++ snd ++ all): _*)
  }

  def visitorsDiv(x: Seq[(String, String, String)]): NodeSeq = x.map {
    case (n, b, c) => Div(Style(Float.Left, BackgroundColor(b), Width(20.px), Color(c)))(t"${n.head}")
  }

  def drawBoard[T <% SquareDecorator](squares: Seq[T]) = {
    def outer(o1: Orientations) = o1.outerEdge(border)
    def tdWithBorders(o: Orientations*) = Td(Style(o.map(outer(_)): _*))
    val O = Orientations
    val top = squares.view(20, 31).force
    val sides = (11 to 19).reverse zip (31 to 40) flatMap (x => Seq(x._1, x._2).map(squares))
    val bottom = squares.view(0, 11).reverse.force
    val it = (top ++ sides ++ bottom).iterator
    def playerCageStyle(others: CssDeclaration*) = Style((Seq(PaddingLeft(3.px), TextAlign.Center, Height(20.px)) ++ others): _*)
    def commonStyles(others: CssDeclaration*) = Style((Seq(Height(80.px), TextAlign.Center) ++ others): _*)
    Table {
      1 to 11 map {
        case 1 =>
          Tr {
            tdWithBorders(O.Top, O.Left) ++ tdWithBorders(O.Top) ++ tdWithBorders(O.Top) ++
              top.view(1, 10).flatMap { s => (1 to 5) map { i => Td(styleHouses(s.houses, i, s.ownerColor, O.Top)) } } ++
              tdWithBorders(O.Top, O.Left) ++ tdWithBorders(O.Top) ++ tdWithBorders(O.Top, O.Right)
          } ++ Tr {
            tdWithBorders(O.Left) ++
              Td(commonStyles())(it.next.toString) ++
              tdWithBorders(O.Right) ++
              (2 to 10).map { _ => Td('colspan -> "5", commonStyles(O.Top.forwardsEdge(border)))(it.next.toString) } ++
              tdWithBorders(O.Left) ++
              Td(commonStyles())(it.next.toString) ++
              tdWithBorders(O.Right)
          } ++ Tr {
            tdWithBorders(O.Left) ++
              Td(playerCageStyle())(visitorsDiv(top.head.visitors)) ++
              Td() ++
              top.view(1, 10).map { s => Td('colspan -> "5", playerCageStyle(BorderLeft(border), BorderRight(border)))(visitorsDiv(s.visitors)) } ++
              Td() ++
              Td(playerCageStyle())(visitorsDiv(top.last.visitors)) ++
              tdWithBorders(O.Right)
          } ++ Tr {
            tdWithBorders(O.Left, O.Bottom) ++ tdWithBorders(O.Bottom) ++ tdWithBorders(O.Bottom) ++
              top.view(1, 10).flatMap { s => (1 to 5) map { i => Td(styleColor(s.color, i, Orientations.Top)) } } ++
              tdWithBorders(O.Bottom) ++ tdWithBorders(O.Bottom) ++ tdWithBorders(O.Bottom, O.Right)
          }
        case 11 =>
          Tr {
            tdWithBorders(O.Left, O.Top) ++ tdWithBorders(O.Top) ++ tdWithBorders(O.Top, O.Right) ++
              bottom.view(1, 10).flatMap { s => (1 to 5).reverse map { i => Td(styleColor(s.color, i, O.Bottom)) } } ++
              tdWithBorders(O.Left, O.Top) ++ tdWithBorders(O.Top) ++ tdWithBorders(O.Top, O.Right)
          } ++ Tr {
            tdWithBorders(O.Left) ++
              Td(commonStyles())(it.next.toString) ++
              tdWithBorders(O.Right) ++
              (2 to 10).map { _ => Td('colspan -> "5", commonStyles(O.Bottom.forwardsEdge(border)))(it.next.toString) } ++
              tdWithBorders(O.Left) ++
              Td(commonStyles())(it.next.toString) ++
              tdWithBorders(O.Right)
          } ++ Tr {
            tdWithBorders(O.Left) ++
              Td(playerCageStyle())(visitorsDiv(bottom.head.visitors)) ++
              Td() ++
              bottom.view(1, 10).map { s => Td('colspan -> "5", playerCageStyle(BorderLeft(border), BorderRight(border)))(visitorsDiv(s.visitors)) } ++
              Td() ++
              Td(playerCageStyle())(visitorsDiv(bottom.last.visitors)) ++
              tdWithBorders(O.Right)
          } ++ Tr {
            tdWithBorders(O.Left, O.Bottom) ++ tdWithBorders(O.Bottom) ++ tdWithBorders(O.Bottom, O.Right) ++
              bottom.view(1, 10).flatMap { s => (1 to 5).reverse map { i => Td(styleHouses(s.houses, i, s.ownerColor, Orientations.Bottom)) } } ++
              tdWithBorders(O.Left, O.Bottom) ++ tdWithBorders(O.Bottom) ++ tdWithBorders(O.Bottom, O.Right)
          }
        case _ =>
          val fst, snd = it.next
          Tr {
            Td(styleHouses(fst.houses, 5, fst.ownerColor, O.Left)) ++
              Td('rowspan -> "4", commonStyles(O.Left.forwardsEdge(border)))(fst.toString) ++
              Td(styleColor(fst.color, 5, O.Left)) ++
              Td('colspan -> (9 * 5).toString, 'rowspan -> "5") ++
              Td(styleColor(snd.color, 1, O.Right)) ++
              Td('rowspan -> "4", commonStyles(O.Right.backwardsEdge(border)))(snd.toString) ++
              Td(styleHouses(snd.houses, 1, snd.ownerColor, O.Right))
          } ++ (2 to 5).map { i =>
            Tr {
              Td(styleHouses(fst.houses, 6 - i, fst.ownerColor, Orientations.Left)) ++
                (if (i == 5) Td(playerCageStyle())(visitorsDiv(fst.visitors)) else NodeSeq.Empty) ++
                Td(styleColor(fst.color, 6 - i, Orientations.Left)) ++
                Td(styleColor(snd.color, i, Orientations.Right)) ++
                (if (i == 5) Td(playerCageStyle())(visitorsDiv(snd.visitors)) else NodeSeq.Empty) ++
                Td(styleHouses(snd.houses, i, snd.ownerColor, Orientations.Right))
            }
          }
      } flatten
    }
  }

  def updateGame(g: Game) = {
    val playerColors: Map[Player, (String, String, String)] = (g.players zip availablePlayerColors).map { case (p, (b, c)) => (p, (p.name, b, c)) }(collection.breakOut)
    val properties: Map[Square, (Player, Int)] = (for {
      p <- g.players
      (s, houses) <- p.properties
    } yield s -> (p, houses))(collection.breakOut)
    g.squares.zipWithIndex map {
      case (s, i) => new SquareDecorator {
        val houses = s match {
          case p: Property =>
            properties.get(p).map(_._2).getOrElse(-2)
          case _ => -2
        }
        val name = s.toString
        val visitors = g.players.filter(_.position == i) map playerColors
        val color = s match {
          case s: Street => s.color.toString.toLowerCase
          case _ => "white"
        }
        val ownerColor = s match {
          case p: Property =>
            val ownerColor = for {
              (owner, _) <- properties.get(p)
              (_, color, _) <- playerColors.get(owner)
            } yield color.toString
            ownerColor getOrElse "white"
          case _ => "white"
        }
      }
    }
  }

  def redraw(g: Game) = {
    val elems = updateGame(g)
    (contentDivId.empty & contentDivId.appendXml(drawBoard(elems))) e
  }

}
