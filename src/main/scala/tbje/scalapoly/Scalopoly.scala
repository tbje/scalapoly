package tbje.scalapoly

import util.Random
import java.io.PrintWriter
import scala.io.Source

case class Player(name: String, properties: Map[Square, Int] = Map(), position: Int = 0, balance: Int = 1500,
  prisonTimeLeft: Int = 0, insurrance: Boolean = false, outOfPrison: Boolean = false)

object Colors extends Enumeration {
  val Brown, LightBlue, Pink, Orange, Red, Yellow, Green, DarkBlue = Value
}
import Colors._

object NormalGame extends App {
  Game.init()
}

object Game extends Squares with ChanceCards {
  type Hook = Game => Unit
  val Empty = Game(Seq(Player("Dummy")))

  def getPlayers(): Seq[String] = {
    val Number = """([1-6])""".r
    readLine("Nb. of players (1-6)?: ") match {
      case Number(nb) =>
        (1 to nb.toInt) map { i => readLine(s"Name player $i?: ") }
      case _ => getPlayers()
    }
  }
  def init(implicit hook: Hook = g => ()): Game = {
    readLine("Welcome to Scalapoly - (N)ew game or (L)oad? ").toLowerCase() match {
      case "l" => load
      case "n" => newGame
      case "q" => Empty
      case _ => init
    }

  }

  def newGame(implicit hook: Hook) = play(Game(util.Random.shuffle(getPlayers()).map(Player(_))))

  val streetsPerColor = squares.collect { case s: Street => s } groupBy (_.color) map { case (a, b) => a -> b.length }

  val housePrices = Map(Brown -> 50, LightBlue -> 50, Pink -> 100, Orange -> 100, Red -> 150, Yellow -> 150, Green -> 200, DarkBlue -> 200)

  def dice = scala.util.Random.nextInt(6) + 1

  def play(g: Game)(implicit hook: Game => Unit = g => ()): Game = {
    hook(g)
    readLine(s"${g.player.name} you're at ${squares(g.player.position)} your balance is ${g.player.balance}$$ > ") match {
      case "s" =>
        g.stats
        play(g)
      case "h" =>
        val h = g.houseMenu
        play(h)
      case "q" =>
        g.save; g
      case "d" =>
        play(g.next)
      case "l" =>
        load(hook)
      case _ =>
        play(g)
    }
  }

  def move(user: Player, first: Int, second: Int, doubles: Int = 0): (Player, Boolean) = {
    def normalMove(user: Player, first: Int, second: Int) = {
      val newPos = user.position + first + second
      if (newPos > 39) {
        user.copy(balance = user.balance + 200, position = newPos - 40)
      } else {
        user.copy(position = newPos)
      }
    }
    if (user.prisonTimeLeft > 0) {
      val outOfPrisonTxt = if (user.outOfPrison) "use out of prison (C)ard, " else ""
      readLine(s"  You're in prison:$outOfPrisonTxt (B)ail - 50 $$ or (D)ice doubles: ").toUpperCase match {
        case "C" if user.outOfPrison => move(user.copy(prisonTimeLeft = 0, outOfPrison = false), first, second)
        case "B" => move(user.copy(prisonTimeLeft = 0, balance = user.balance - 50), first, second)
        case "D" =>
          if (first == second) {
            normalMove(user.copy(prisonTimeLeft = 0), first, second) -> false
          } else if (user.prisonTimeLeft == 1) {
            user.copy(balance = user.balance - 50, prisonTimeLeft = 0) -> false
          } else {
            user.copy(prisonTimeLeft = user.prisonTimeLeft - 1) -> false
          }
        case _ => move(user, first, second, doubles)
      }
    } else {
      if (doubles == 2 && first == second) {
        user.copy(position = squares.indexOf(VisitingJail), prisonTimeLeft = 3) -> false
      } else {
        normalMove(user, first, second) -> (first == second)
      }
    }
  }

  def load(implicit hook: Hook): Game = {
    import ScalapolyJsonProtocol._
    import spray.json._
    try {
      def jsonToGame(json: String) = {
        val parsed = JsonParser(json)
        val g = parsed.convertTo[Game]
        play(g)
      }
      val f = new java.io.File("savegame")
      if (f.exists && f.isFile) {
        jsonToGame(Source.fromFile(f).getLines().mkString("\n"))
      } else {
        readLine("Paste some valid json to load game (q - to quit): ") match {
          case "q" => init
          case json => jsonToGame(json)
        }
      }
    } catch {
      case e: Exception => load(hook)
    }
  }

  def ownedInColor(player: Player, color: Colors.Value): Map[Street, Int] =
    player.properties.collect { case (street @ Street(_, `color`, _, _, _), houses) => street -> houses }

  def placeEvenly(player: Player, street: Street, houses: Int): Map[Square, Int] = {
    val owned = ownedInColor(player, street.color)
    val res: Map[Int, Int] = placeEvenly(squares.indexOf(street), houses, owned.map(x => squares.indexOf(x._1) -> x._2))
    val additions = res.map(x => squares(x._1) -> x._2)
    player.properties ++ additions
  }

  def placeEvenly(prefered: Int, houses: Int, all: Map[Int, Int]): Map[Int, Int] = if (houses == 0) all else {
    val highestIndexAfterPrefered = (all - prefered).keys.max
    def incremented(key: Int) = key -> (all(key) + 1)
    val mostHouses = all.values.max
    val allSame = all.values.forall(_ == mostHouses)
    val allNext = if (allSame || all(prefered) != all(highestIndexAfterPrefered)) {
      all + incremented(prefered)
    } else if (all(prefered) == mostHouses && all(highestIndexAfterPrefered) == mostHouses) {
      all + incremented((all - prefered - highestIndexAfterPrefered).keys.head)
    } else { // if (all(prefered) == max) {
      all + incremented(highestIndexAfterPrefered)
    }
    placeEvenly(prefered, houses - 1, allNext)
  }

  def withResource[T, R <: { def close(): Unit }](r: R)(block: R => T) =
    try { block(r) } finally r.close()
}

case class Game(players: Seq[Player], turn: Int = 0, doubleCount: Int = 0) extends Squares {
  import Game._
  val playerIndex = turn % players.length
  val player = players(playerIndex)

  def save = {
    import ScalapolyJsonProtocol._
    import spray.json._
    withResource(new PrintWriter("savegame")) { writer =>
      writer.println(this.toJson)
      writer.flush()
    }
    println(this.toJson)
  }

  def canBuyHouses(player: Player, street: Street) = {
    val owned = ownedInColor(player, street.color)
    if (owned.size == streetsPerColor.getOrElse(street.color, throw new Exception(s"could not find ${street.color} in $streetsPerColor"))) {
      if (owned.forall(_._2 >= 0)) {
        if (owned(street) < 5) {
          None
        } else {
          Some("To many houses")
        }
      } else {
        Some("Some are mortgaged")
      }
    } else {
      Some("You don't own all houses")
    }
  }

  def placeEvenly(street: Street, houses: Int) = Game.placeEvenly(player, street, houses)

  def houseMenu: Game = {
    val userStreets: Seq[(Street, Int)] = player.properties collect { case (s: Street, h) => s -> h } toSeq
    val userStreetsByColor = userStreets groupBy { case (s, h) => s.color }
    val userStreetsPerColor = userStreetsByColor map { case (c, l) => c -> l.length }
    userStreetsPerColor.foreach {
      case (color, number) =>
        val neededForHouses = streetsPerColor.getOrElse(color, throw new Exception(s"could not find ${color} in $streetsPerColor"))
        println(s"$color ($number / $neededForHouses):")
        val canBuyHouses = number == neededForHouses
        userStreetsByColor(color) foreach {
          case (s, b) =>
            val buyStr = if (canBuyHouses) s"(b)uy ${housePrices(s.color)}" else ""
            val (status, buy) = b match {
              case -1 => "mortgaged" -> s"un(m)ortgage ${(s.mortgage * 1.1).toInt}"
              case 0 => "" -> (s"(m)ortage ${s.mortgage} " + buyStr)
              case 5 => "with hotel" -> s"(s)ell"
              case 1 => "with 1 house" -> s"$buyStr (s)ell"
              case x => s"with $x houses" -> s"$buyStr (s)ell"
            }
            println(s"  $s $status: ${squares.indexOf(s)} $buy")
        }
    }
    val Mortage = """(\d{1,2}) m""".r
    val Buy = """(\d{1,2}) b (\d{1,2})""".r
    val Sell = """(\d{1,2}) s (\d{1,2})""".r
    def repeat: Game = {
      readLine("(id) > ") match {
        case Mortage(id) =>
          squares.lift(id.toInt) match {
            case Some(street: Property) if player.properties.contains(street) =>
              println(s"  $street mortaged, you have ${player.balance + street.mortgage}$$ in the bank.")
              Game(players.replace(player, p => p.copy(balance = p.balance + street.mortgage, properties = p.properties + (street -> -1))), turn, doubleCount)
            case Some(street: Property) =>
              println(s"  you don't own $street"); repeat
            case Some(street) =>
              println(s"  $street is not a property"); repeat
            case None => println(s"  $id is not a valid id"); repeat
          }
        case Buy(id, number) =>
          val houses = number.toInt
          squares.lift(id.toInt) match {
            case Some(street: Street) =>
              canBuyHouses(player, street) match {
                case None =>
                  val total = housePrices(street.color) * houses
                  if (total <= player.balance) {
                    Game(players.replace(player, p => p.copy(balance = p.balance - total, properties = placeEvenly(street, houses))), turn, doubleCount)
                  } else {
                    println("  not enough money")
                    this
                  }
                case Some(problem) =>
                  println(problem)
                  this
              }
            case Some(notStreet) =>
              println("  Not a valid street")
              this
            case None => this
          }
        case Sell(id, number) => this
        case "" | "q" => this
        case _ => println("  did not understand"); repeat
      }
    }
    repeat
  }

  def next(implicit hook: Hook): Game = {
    val first, second = dice
    val (afterDice, double) = Game.move(player, first, second, doubleCount)
    hook(this.copy(players.replace(player, _ => afterDice)))
    println(s"  ${player.name}, you diced $first, $second and are now at ${squares(afterDice.position)}.")
    val newPlayers = squares(afterDice.position).onArrival(first + second, afterDice, players.updated(playerIndex, afterDice))
    val g = if (double) Game(newPlayers, turn, doubleCount + 1) else Game(newPlayers, turn + 1)
    g
  }

  def stats = {
    players foreach { p =>
      println(s"${p.name} is at ${squares(p.position)} and has ${p.balance}$$")
      for {
        (square, houses) <- p.properties
      } println(s"  owns $square with $houses houses.")
    }
  }
}

