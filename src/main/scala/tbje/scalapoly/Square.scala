package tbje.scalapoly

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

case object GoToJail extends Square with Board {
  def userFunc(user: Player) = user.copy(position = board.indexOf(VisitingJail), prisonTimeLeft = 3)
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
    card.f(player, players)
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

