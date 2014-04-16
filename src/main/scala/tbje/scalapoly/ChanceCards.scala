package tbje.scalapoly

case class Card(text: String, f: Player => Player = x => x, all: Seq[Player] => Seq[Player] = x => x)

trait ChanceCards extends Squares {
  val chanceCards = Seq(
    Card("Advance to Go (Collect $200)", u => u.copy(balance = u.balance + 200, position = 0)),
    Card("Bank error in your favor – collect $75", u => u.copy(balance = u.balance + 75)),
    Card("Doctor's fees – Pay $50", u => u.copy(balance = u.balance - 50)),
    Card("Get out of jail free – this card may be kept until needed, or sold", u => u.copy(outOfPrison = true)),
    Card("Go to jail – go directly to jail – Do not pass Go, do not collect $200", GoToJail.userFunc),
    Card("It is your birthday Collect $10 from each player"),
    Card("Grand Opera Night – collect $50 from every player for opening night seats"),
    Card("Income Tax refund – collect $20", u => u.copy(balance = u.balance + 20)),
    Card("Life Insurance Matures – collect $100", u => u.copy(balance = u.balance + 100)),
    Card("Pay Hospital Fees of $100", u => u.copy(balance = u.balance - 100)),
    Card("Pay School Fees of $50", u => u.copy(balance = u.balance - 50)),
    Card("Receive $25 Consultancy Fee", u => u.copy(balance = u.balance + 25)),
    Card("You are assessed for street repairs – $40 per house, $115 per hotel"),
    Card("You have won second prize in a beauty contest– collect $10", u => u.copy(balance = u.balance + 10)),
    Card("You inherit $100", u => u.copy(balance = u.balance + 100)),
    Card("From sale of stock you get $50", u => u.copy(balance = u.balance + 50)),
    Card("Holiday Fund matures - Receive $100", u => u.copy(balance = u.balance + 100)),
    Card("Advance to Go (Collect $200)", u => u.copy(position = findIndex(Go), balance = u.balance + 200)),
    Card("Advance to Trafalgar Square.", u => u.copy(position = byName("Trafalgar Square").map(findIndex).getOrElse(u.position))),
    Card("Advance token to nearest Utility. If unowned, you may buy it from the Bank. If owned, throw dice and pay owner a total ten times the amount thrown."),
    Card("Advance token to the nearest Railroad and pay owner twice the rental to which he/she is otherwise entitled. If Railroad is unowned, you may buy it from the Bank. (There are two of these.)"),
    Card("Advance to Pall Mall – if you pass Go, collect $200"),
    Card("Bank pays you dividend of $50", u => u.copy(balance = u.balance + 50)),
    Card("Get out of Jail free – this card may be kept until needed, or traded/sold"),
    Card("Go back 3 spaces", u => u.copy(position = if (u.position - 3 >= 0) u.position - 3 else u.position + 40 - 3)),
    Card("Go directly to Jail – do not pass Go, do not collect $200"),
    Card("Make general repairs on all your property – for each house pay $25 – for each hotel $100"),
    Card("Pay poor tax of $15", u => u.copy(balance = u.balance - 15)),
    Card("Take a trip to Reading Railroad – if you pass Go collect $200"),
    Card("Advance token to Mayfair", u => u.copy(position = 39)),
    Card("You have been elected chairman of the board – pay each player $50"),
    Card("Your building loan matures – collect $150", u => u.copy(balance = u.balance + 150)),
    Card("You have won a crossword competition - collect $10", u => u.copy(balance = u.balance + 10)))
}
