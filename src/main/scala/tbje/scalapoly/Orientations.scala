package tbje.scalapoly

import tbje.facelift.imports._
import tbje.facelift.css.CssMeasure

trait Orientations {
  val boardEdge: String => tbje.facelift.css.CssDeclaration
  val outerEdge: String => tbje.facelift.css.CssDeclaration
  val forwardsEdge: String => tbje.facelift.css.CssDeclaration
  val backwardsEdge: String => tbje.facelift.css.CssDeclaration
  val height: CssMeasure => tbje.facelift.css.CssDeclaration
  val width: CssMeasure => tbje.facelift.css.CssDeclaration
}
object Orientations {
  object Left extends Orientations {
    val boardEdge = BorderRight.apply _
    val outerEdge = BorderLeft.apply _
    val forwardsEdge = BorderTop.apply _
    val backwardsEdge = BorderBottom.apply _
    val height = (m: CssMeasure) => Width(m)
    val width = (m: CssMeasure) => Height(m)
  }
  object Right extends Orientations {
    val boardEdge = BorderLeft.apply _
    val outerEdge = BorderRight.apply _
    val forwardsEdge = BorderBottom.apply _
    val backwardsEdge = BorderTop.apply _
    val height = (m: CssMeasure) => Width(m)
    val width = (m: CssMeasure) => Height(m)
  }
  object Top extends Orientations {
    val boardEdge = BorderBottom.apply _
    val outerEdge = BorderTop.apply _
    val forwardsEdge = BorderRight.apply _
    val backwardsEdge = BorderLeft.apply _
    val height = (m: CssMeasure) => Height(m)
    val width = (m: CssMeasure) => Width(m)
  }
  object Bottom extends Orientations {
    val boardEdge = BorderTop.apply _
    val outerEdge = BorderBottom.apply _
    val forwardsEdge = BorderLeft.apply _
    val backwardsEdge = BorderRight.apply _
    val height = (m: CssMeasure) => Height(m)
    val width = (m: CssMeasure) => Width(m)
  }
}
