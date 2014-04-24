package tbje.scalapoly

import org.specs2.matcher.Matchers
import org.specs2.mutable._

class GameSpec extends Specification with Matchers with Squares {
  //      squares.zipWithIndex foreach { case (s : Street, i) => println(s"$i - $s ${s.color}"); case _ =>  }
  def createOwner(street: Int, houses: Int = 0) = squares(street) -> houses

  "Game.ownedInColor" should {
    "return correct values for one yellow (one)" in {
      Game.ownedInColor(Player("", Map(createOwner(26, 2))), Colors.Yellow) === Map(squares(26) -> 2)
    }
    "return correct values for one yellow (three)" in {
      Game.ownedInColor(Player("", Map(createOwner(26, 2), createOwner(27, 2), createOwner(29, 2))), Colors.Yellow) === Map(squares(26) -> 2, squares(27) -> 2, squares(29) -> 2)
    }
    "return correct values for one lightBlue (none)" in {
      Game.ownedInColor(Player("", Map(squares(26) -> 0)), Colors.LightBlue) === Map()
    }
  }

  "Game.placeEvenly" should {
    "return correct values for 5 houses on 26 yellow (three)" in {
      Game.updateEvenly(Game.insertEvenly)(Player("", Map(createOwner(26, 0), createOwner(27, 1), createOwner(29, 0))), squares(26).asInstanceOf[Street], 5) === Map(squares(26) -> 2, squares(27) -> 2, squares(29) -> 2)
    }
  }

  "Game.placeEvenly" should {
    "return correct values for 5 houses on 26 yellow (three)" in {
      Game.insertEvenly(2, 0, Map(2 -> 0, 3 -> 0, 4 -> 0)) === Map(2 -> 0, 3 -> 0, 4 -> 0)
    }
    "return correct values for 5 houses on 26 yellow (three)" in {
      Game.insertEvenly(2, 1, Map(2 -> 0, 3 -> 0, 4 -> 0)) === Map(2 -> 1, 3 -> 0, 4 -> 0)
    }
    "return correct values for 5 houses on 26 yellow (three)" in {
      Game.insertEvenly(2, 2, Map(2 -> 0, 3 -> 0, 4 -> 0)) === Map(2 -> 1, 3 -> 0, 4 -> 1)
    }
    "return correct values for 5 houses on 26 yellow (three)" in {
      Game.insertEvenly(2, 3, Map(2 -> 0, 3 -> 0, 4 -> 0)) === Map(2 -> 1, 3 -> 1, 4 -> 1)
    }
    "return correct values for 5 houses on 26 yellow (three)" in {
      Game.insertEvenly(2, 2, Map(2 -> 0, 3 -> 1, 4 -> 1)) === Map(2 -> 2, 3 -> 1, 4 -> 1)
    }
    "return correct values for 5 houses on 26 yellow (three)" in {
      Game.insertEvenly(4, 2, Map(2 -> 0, 3 -> 1, 4 -> 1)) === Map(2 -> 1, 3 -> 1, 4 -> 2)
    }
    "return correct values for 5 houses on 26 yellow (three)" in {
      Game.insertEvenly(4, 9, Map(2 -> 0, 3 -> 1, 4 -> 1)) === Map(2 -> 3, 3 -> 4, 4 -> 4)
    }
  }
}

