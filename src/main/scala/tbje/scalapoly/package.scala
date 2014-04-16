package tbje

package object scalapoly {
  implicit class PlayersOps2(val p: Seq[Player]) extends AnyVal {
    def replace(oldP: Player, f: Player => Player) =
      p.updated(p.indexOf(oldP), f(oldP))
  }
}

