package tbje.scalapoly
import Colors._

trait Square {
  def onArrival(dice: Int, user: Player, players: Seq[Player]): Seq[Player] = players
}

trait Property extends Square {
  def name: String
  override val toString = name
  def cost: Int
  def mortgage: Int = cost / 2
  def price(lastDice: Int, houses: Int, owner: Player): Int
  def welcomeHome(name: String, playerName: String): String
  def owned(owner: Player, houses: Int, price: Int): String
  override def onArrival(dice: Int, player: Player, players: Seq[Player]): Seq[Player] = {
    val playerIndex = players.indexOf(player)
    val allProperties: Map[Square, (Player, Int)] = (for {
      p <- players
      (property, houses) <- p.properties
    } yield property -> (p -> houses))(collection.breakOut)
    allProperties.get(this) match {
      case Some((`player`, _)) =>
        println(welcomeHome(name, player.name))
        players
      case Some((player, -1)) =>
        println(s"  $name mortgaged by ${player.name}, free passage.")
        players
      case Some((owner, houses)) =>
        val calculatedPrice = price(dice, houses, owner)
        println(owned(owner, houses, calculatedPrice))
        val newPlayers = players.replace(owner, o => o.copy(balance = owner.balance + calculatedPrice))
          .replace(player, p => p.copy(balance = p.balance - calculatedPrice))
        newPlayers
      case None =>
        readLine(s"  Would you like to buy $name for $cost$$? (y/n): ").toLowerCase match {
          case "y" =>
            players.replace(player, p => p.copy(balance = p.balance - cost, properties = p.properties + (this -> 0)))
          case "n" =>
            players
          case _ =>
            onArrival(dice, player, players)
        }
    }
  }
}

case class Street(name: String, color: Colors.Value, cost: Int, override val mortgage: Int, rent: List[Int]) extends Square with Property {
  def price(lastDice: Int, houses: Int, owner: Player): Int =
    rent(houses)
  def welcomeHome(name: String, playerName: String): String =
    s"  Welcome home to $name, $playerName."
  override def owned(owner: Player, houses: Int, price: Int): String = houses match {
    case 0 =>
      s"  ${owner.name} owns $name. That will be $price$$ please."
    case 5 =>
      s"  ${owner.name} owns $name with an hotel, that will be $price$$ please."
    case x =>
      s"  ${owner.name} owns $name, with $houses houses, that will be $price$$ please."
  }
}

case class Station(name: String) extends Square with Property {
  val cost = 200
  val prices = Seq(25, 50, 100, 200)
  def others(player: Player) =
    player.properties collect { case (s: Station, state) if s != this & state != -1 => s } toSeq
  def price(lastDice: Int, houses: Int, owner: Player): Int =
    prices(others(owner).length)
  def welcomeHome(name: String, playerName: String): String =
    s"  Welcome to your owners longue here at $name, $playerName."
  def owned(owner: Player, houses: Int, price: Int): String = {
    val and = Utils.and(name +: others(owner).map(_.name): _*)
    s"  ${owner.name} owns $and. That will be $price$$ please."
  }
}

object Utils {
  def and[T](s: T*): String = s match {
    case Nil => ""
    case t +: Nil => t.toString
    case fst :+ lst => fst.mkString(", ") + " and " + lst
  }
}

case class Utilities(name: String) extends Square with Property {
  val cost = 150
  def others(player: Player) =
    player.properties collect { case (s: Utilities, state) if s != this & state != -1 => s } toSeq
  def price(lastDice: Int, houses: Int, owner: Player): Int =
    (if (others(owner).length == 1) 10 else 4) * lastDice
  def welcomeHome(name: String, playerName: String): String =
    s"  Welcome $playerName, what can we do for you today at $name."
  def owned(owner: Player, houses: Int, price: Int): String = {
    val and = Utils.and(name +: others(owner).map(_.name): _*)
    s"  ${owner.name} owns $and. That will be $price$$ please."
  }
}

case object GoToJail extends Square with Squares {
  def userFunc(user: Player) = user.copy(position = squares.indexOf(VisitingJail), prisonTimeLeft = 3)
  override def onArrival(dice: Int, player: Player, players: Seq[Player]) = {
    println(s"  ${player.name}, please go to jail!")
    players.replace(player, userFunc)
  }
}

case object Chance extends Square with ChanceCards {
  override def onArrival(dice: Int, player: Player, players: Seq[Player]): Seq[Player] = {
    val randIndex = util.Random.nextInt(chanceCards.length)
    val card = chanceCards(randIndex)
    println(s"  ${card.text}")
    players.replace(player, card.f)
  }
}

case object FreeParking extends Square

case object Tax extends Square {
  override def onArrival(dice: Int, player: Player, players: Seq[Player]) = {
    print(s"  ${player.name}, please pay regular tax 200$$(a) or 10%(b) please: ")
    readLine() match {
      case "a" =>
        players.replace(player, x => x.copy(balance = x.balance - 200))
      case "b" => // TODO calculate full worth
        players.replace(player, x => x.copy(balance = x.balance - (x.balance * 0.1).toInt))
      case _ => onArrival(dice, player, players)
    }
  }
}

case object SuperTax extends Square {
  override def onArrival(dice: Int, player: Player, players: Seq[Player]) = {
    println(s"  ${player.name}, please pay super tax 100$$ please.")
    players.replace(player, x => x.copy(balance = x.balance - 100))
  }
}

case object Go extends Square

case object VisitingJail extends Square

trait Squares {
  def findIndex(x: Square) = squares.indexOf(x)
  def byName(name: String) = squares collect { case p: Property if p.name == name => p } headOption

  val squares = Seq[Square](
    Go,
    Street("Old Kent Road", Brown, 60, 30, List(2, 10, 30, 90, 160, 250)),
    Chance,
    Street("Whitechapel Road", Brown, 60, 30, List(4, 20, 60, 180, 320, 450)),
    Tax,
    Station("Kings Cross Station"),
    Street("The Angel Islington", LightBlue, 100, 50, List(6, 30, 90, 270, 400, 550)),
    Chance,
    Street("Euston Road", LightBlue, 100, 50, List(6, 30, 90, 270, 400, 550)),
    Street("Pentonville Road", LightBlue, 120, 60, List(8, 40, 100, 300, 450, 600)),
    VisitingJail,
    Street("Pall Mall", Pink, 140, 70, List(10, 50, 150, 450, 625, 750)),
    Utilities("Electric Company"),
    Street("Whitehall", Pink, 140, 70, List(10, 50, 150, 450, 625, 750)),
    Street("Northumberland Avenue", Pink, 160, 80, List(12, 60, 180, 500, 700, 900)),
    Station("Marylebone Station"),
    Street("Bow Street", Orange, 180, 90, List(14, 70, 200, 550, 750, 950)),
    Chance,
    Street("Marlborough Street", Orange, 180, 90, List(14, 70, 200, 550, 750, 950)),
    Street("Vine Street", Orange, 200, 100, List(16, 80, 220, 600, 800, 1000)),
    FreeParking,
    Street("The Strand", Red, 220, 110, List(18, 90, 250, 700, 875, 1050)),
    Chance,
    Street("Fleet Street", Red, 220, 110, List(18, 90, 250, 700, 875, 1050)),
    Street("Trafalgar Square", Red, 240, 120, List(20, 100, 300, 750, 925, 1100)),
    Station("Fenchurch St Station"),
    Street("Leicester Square", Yellow, 260, 130, List(22, 110, 330, 800, 975, 1150)),
    Street("Coventry Street", Yellow, 260, 130, List(22, 110, 330, 800, 975, 1150)),
    Utilities("Water Works"),
    Street("Piccadilly", Yellow, 280, 140, List(22, 120, 360, 850, 1025, 1200)),
    GoToJail,
    Street("Regent Street", Green, 300, 150, List(26, 130, 390, 900, 1100, 1275)),
    Street("Oxford Street", Green, 300, 150, List(26, 130, 390, 900, 1100, 1275)),
    Chance,
    Street("Bond Street", Green, 320, 160, List(28, 150, 450, 1000, 1200, 1400)),
    Station("Liverpool Street Station"),
    Chance,
    Street("Park Lane", DarkBlue, 350, 175, List(35, 175, 500, 1100, 1300, 1500)),
    SuperTax,
    Street("Mayfair", DarkBlue, 400, 200, List(50, 200, 600, 1400, 1700, 2000)))
}
