package tbje.web.util

import scala.xml._
import scala.collection.JavaConversions._
import net.liftweb.util.Helpers._
import net.liftweb.http.TemplateFinder
import net.liftweb.util.PCDataXmlParser
import net.liftweb.common.Empty
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsExp
import net.liftweb.http.js.JsCmds.Script
import tbje.facelift.imports._
import tbje.web.util.JsUtil._

object HtmlUtil extends Logger {

  def makeChangeBlur(in: Elem): Elem = {
    in.attribute("onblur") match {
      case Some(ns) => in % ("onchange" -> ns)
      case _ => in
    }
  }

  def editSave(currentValue: String)(saveFunc: String => Unit): Elem = {
    // val (rs, sid) = findOrAddId(shown)
    // val (rh, hid) = findOrAddId(hidden)
    // val ui = LiftRules.jsArtifacts
    // (<span>{rs % ("onclick" -> (ui.hide(sid).cmd &
    //         ui.showAndFocus(hid).cmd & JsRaw("return false;")))}{dealWithBlur(rh % ("style" -> "display: none"), (ui.show(sid).cmd & ui.hide(hid).cmd))}</span>)
    Span(currentValue)(A(Href("")("Edit")))
  }

  def decomposeMetaData(m: MetaData): Option[(String, String)] = m match {
    case Null => None
    case PrefixedAttribute(pre, key, values, next) =>
      Some(pre + ":" + key -> values.mkString(" "))
    case UnprefixedAttribute(key, values, next) =>
      Some(key -> values.mkString(" "))
  }

  def unchainMetaData(n: Node): Map[String, String] = (n match {
    case Elem(_, _, attr, _, _) => attr.toList.flatMap(decomposeMetaData(_))
    case _ => Nil
  }).toMap

  def unchainMetaData(m: MetaData): Map[String, String] = m.flatMap(decomposeMetaData(_)).toMap

  def chainMetaData(map: Map[String, String]): MetaData = chainMetaData(map.toList)

  def chainMetaData(map: List[(String, String)]): MetaData = map.toList match {
    case Nil => Null
    case (key, value) :: Nil => Attribute(None, key, Text(value) :: Nil, Null)
    case (key, value) :: next => Attribute(None, key, Text(value) :: Nil, chainMetaData(next))
    case x => Null
  }

  def attr(x: (String, String)) = new UnprefixedAttribute(x._1, x._2, scala.xml.Null)
  def addOptAttr(e: Elem, a: Option[(String, String)]) = a.map(a => e % attr(a)).getOrElse(e)
  def capEach(x: String) = ((x toLowerCase) split " " map (_ capitalize)).mkString(" ")

  def appendAttr(n: Node, attr: (String, String)) = {
    n match {
      case Elem(pre, name, attrs, x, content @ _*) =>
        new Elem(pre, name, chainMetaData(mergeValue(unchainMetaData(attrs), attr)), x, content.isEmpty, content: _*)
      case x => x
    }
  }

  def replaceAttrValue(n: Elem, attrName: String, replaceFunc: String => String) = {
    n match {
      case Elem(pre, name, attrs, x, content @ _*) =>
        new Elem(pre, name, chainMetaData(replaceValue(unchainMetaData(attrs), attrName, replaceFunc)), x, content.isEmpty, content: _*)
      case x => x
    }
  }

  def bindSnippets(xml: NodeSeq, name: String = "undefined"): NodeSeq = {
    import net.liftweb.http.S
    S.session.map(_.runTemplate(name, xml)) getOrElse NodeSeq.Empty
  }

  def replaceValue(map: Map[String, String], x: String, func: String => String): Map[String, String] = {
    map.get(x) match {
      case Some(v) => (map - x) + (x -> func(v))
      case None => map
    }
  }

  def mergeValue(map: Map[String, String], x: (String, String)): Map[String, String] = {
    map.get(x._1) match {
      case Some(v) => (map - x._1) + (x._1 -> (v + " " + x._2))
      case None => map + (x._1 -> x._2)
    }
  }

  def classMerge(node: Node, className: String) = appendAttr(node, "class" -> className)

  //mergeValue(Map("xx"->"yy", "aa"-> "bb"), "aa"->"ff")
  //appendAttr(<h f="1" g="xx" h="ee">xxx</h>:Node, "2")

  def getTemplate(templateName: String): NodeSeq = {
    val name = templateName.split("/").toList
    val firstTry = TemplateFinder(name)
    val secondTry = Empty // findAnyTemplate("templates-hidden" :: name)
    firstTry or secondTry openOr errorFindingTemplate(templateName)
  }

  def errorFindingTemplate(templateName: String): NodeSeq = {
    logger.error("Could not find: " + templateName)
    <div id="sync-popup-window">
      <div class="roundedbox">
        <div class="roundedbox-content">
          <div class="roundedbox-top"></div>
          <h2>Error</h2>
          <p>An error has occured. We will shortly investigate and correct the problem.</p>
        </div>
        <div class="roundedbox-bottom"><div></div></div>
      </div>
    </div>
  }

  def strToNodeSeq(str: String): NodeSeq = PCDataXmlParser("<x>" + str + "</x>").get.head.child

  def ?(str: String, params: Any*): NodeSeq = {
    import net.liftweb.http.S
    strToNodeSeq(S.?(str, params: _*))
  }

  def odd[T](lst: Seq[T], funOdd: T => T, funEven: T => T = (x: T) => x): List[T] = lst match {
    case Nil => Nil
    case a :: Nil => funOdd(a) :: Nil
    case a :: b :: c => funOdd(a) :: funEven(b) :: odd(c, funOdd, funEven)
  }

  def oddEvenStyles(lst: NodeSeq, oddClass: String = "odd", evenClass: String = "even") =
    odd[Node](lst.toList, x => classMerge(x, oddClass), x => classMerge(x, evenClass))

  // mapXml[Int](lst=List(1,2,3,4,5), fun=x=>(<b>{x}</b>), surround=x=>(oddEvenStyles(x)))	 

  implicit class XmlInterpolation(s: StringContext) {
    import scala.xml._
    object t {
      def apply(exprs: Any*) = Text(s.s(exprs: _*))
    }
  }

  object HeadScript {
    import net.liftweb.http.js.JsCmds.jsExpToJsCmd
    def apply(script: String) = <head_merge>{ Script(jsExpToJsCmd(JsRaw(script))) }</head_merge>
  }

  object XmlGroup {
    def apply(nodeSeq: NodeSeq) = <xml:group>{ nodeSeq }</xml:group>
  }

  class Spinner(idName: String, params: xml.Attribute*) extends JQuery(stringToJQueryId(idName)) {
    val id = Id(idName)
    import tbje.facelift.html._
    import tbje.facelift.attr._
    val html = Img((Src("/img/ajax-loader-trans.gif") +: id +: params): _*)
  }

  import tbje.facelift.attr.AttributeBase
  import net.liftweb.http.CometActor

  case class DataLift(id: String) extends AttributeBase("data-lift", id)

  object DataLiftSnippet {
    def apply[T](c: Class[T]) = DataLift(c.getName().replace("tbje.web.snippet.", ""))
  }

  object LiftComet {
    def apply[T <: CometActor](c: Class[T]) = DataLift("comet?type=" + c.getName().replace("tbje.web.comet.", ""))
  }

}

class HtmlSeq[T](seq: Seq[T]) {
  def mapXml(
    fun: T => NodeSeq,
    sep: NodeSeq = NodeSeq.Empty,
    ifEmpty: NodeSeq = NodeSeq.Empty,
    surround: NodeSeq => NodeSeq = x => x) = seq match {
    case Nil => ifEmpty
    case head :: Nil => surround(fun(head))
    case _ => surround(seq.tail.foldLeft(fun(seq.head))(_ ++ sep ++ fun(_)))
  }
}

object HtmlSeq {
  implicit def seqToHtmlSeq[T](x: Seq[T]) = new HtmlSeq[T](x)
}
