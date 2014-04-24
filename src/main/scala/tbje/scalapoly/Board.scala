package tbje.scalapoly
import scala.reflect.ClassTag
import Colors._

trait Board {
  def findIndex(x: Square) = board.indexOf(x)
  def byName(name: String) = board collect { case p: Property if p.name == name => p } headOption

  val board = Seq[Square](
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
