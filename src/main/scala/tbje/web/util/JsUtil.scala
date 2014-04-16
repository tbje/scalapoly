package tbje.web.util

import scala.reflect.ClassTag

object JsUtil {

  import tbje.web.util.HtmlUtil._
  import tbje.facelift.attr.Class
  import tbje.facelift.attr.Id
  import tbje.facelift.css.CssDeclaration
  import scala.xml.NodeSeq
  import net.liftweb.util.StringHelpers.encJs
  import net.liftweb.http.js.{ JsExp, JsCmd }
  import net.liftweb.http.js.JE._

  implicit def jsExpToJsCmd(in: JsExp) = in.cmd

  implicit class JSInterpolation(s: StringContext) {
    object js {
      def apply(exprs: Any*) = JsRaw(s.s(exprs: _*))
    }
    object jexp {
      def apply(exprs: Any*) = new JsRaw(s.s(exprs: _*)) with JsExp
    }
  }

  object JQuery {
    def apply(x: String) = new JQuery(s"""$$("$x")""")
    def dynLoadScript(scriptPath: String, onFinished: JsExp) = js"""$$.getScript("$scriptPath", function(){ ${onFinished.toJsCmd} })"""
    def dynLoadScript(scriptPath: String): JsRaw = js"""$$.getScript("$scriptPath")"""
    class WhenDone(what: String) {
      def done(onFinished: JsExp): JsRaw = js"""$what.done(${onFinished.toJsCmd})"""
    }
    def when(defered: JsExp*) = {
      new WhenDone(s"""$$.when(${defered.map(_.toJsCmd).mkString(",")})""")
    }
    import net.liftweb.http.js.JsCmds
    import net.liftweb.http.js.JE.AnonFunc
    def onStart(cmds: JsCmd) = jexp"""$$(${AnonFunc(cmds).toJsCmd})"""
  }

  class JQuery(what: String) {
    def hide(): JsCmd = js"""$what.hide()"""
    def show(): JsCmd = js"""$what.show()"""
    def css(property: String): JsCmd = js"""$what.css("$property")"""
    def css(property: String, value: String): JsCmd = js"""$what.css("$property", "$value")"""
    def css(property: String, exp: JsExp): JsCmd = js"""$what.css("$property", ${exp.toJsCmd})"""
    def css(css: CssDeclaration*): JsCmd = js"""$what.css(${css.map { case CssDeclaration(prop, value) => s""""$prop": "$value"""" }.mkString("{", ",", "}")})"""
    def cssInspect(dec: CssDeclaration): JsCmd = js"""$what.css("${dec.property}")"""
    def animate(css: CssDeclaration*): JsCmd = js"""$what.animate(${css.map { case CssDeclaration(prop, value) => s""""$prop": "$value"""" }.mkString("{", ",", "}")})"""
    def animate(time: Int, css: CssDeclaration*): JsCmd = js"""$what.animate(${css.map { case CssDeclaration(prop, value) => s""""$prop": "$value"""" }.mkString("{", ",", "}")}, $time)"""
    def c(method: String, params: String*): JsCmd = js"""$what.$method(${params.mkString("\"", "\",\"", "\"")})"""
    def c(method: String): JsCmd = js"""$what.$method()"""
    def appendXml(xml: NodeSeq): JsCmd = js"""$what.append(${encJs(xml.toString)})"""
    def replaceWithXml(xml: NodeSeq): JsCmd = js"""$what.replaceWith(${encJs(xml.toString)})"""
    def prependXml(xml: NodeSeq): JsCmd = js"""$what.prepend(${encJs(xml.toString)})"""
    def afterXml(xml: NodeSeq): JsCmd = js"""$what.after(${encJs(xml.toString)})"""
    def beforeXml(xml: NodeSeq): JsCmd = js"""$what.before(${encJs(xml.toString)})"""
    def empty: JsCmd = js"""$what.empty()"""
    def serialize: JsCmd = js"""$what.serialize()"""
    def fadeOut: JsCmd = js"""$what.fadeOut()"""
    def fadeOut(onFinished: JsExp): JsCmd = fadeOut(onFinished.cmd)
    def fadeOut(onFinished: JsCmd): JsCmd = js"""$what.fadeOut(400, ${AnonFunc(onFinished).toJsCmd})"""
    def fadeIn: JsCmd = js"""$what.fadeIn()"""
    def fadeIn(onFinished: JsExp): JsCmd = fadeIn(onFinished.cmd)
    def fadeIn(onFinished: JsCmd): JsCmd = js"""$what.fadeIn(400, ${AnonFunc(onFinished).toJsCmd})"""
    def html(xml: NodeSeq): JsCmd = js"""$what.html(${encJs(xml.toString)})"""
    def html(js: JsExp): JsCmd = js"""$what.html(${js.toJsCmd})"""
    def attr[X: ClassTag](attrs: (String, String)*): JsCmd = js"""$what.attr(${attrs.map { case (attr, value) => s""""$attr": "$value"""" }.mkString("{", ",", "}")})"""
    def attr(attrs: (String, JsCmd)*): JsCmd = js"""$what.attr(${attrs.map { case (attr, value) => s""""$attr": ${encJs(value.toJsCmd)}""" }.mkString("{", ",", "}")})"""
    def addClass[X: ClassTag](classes: String*): JsCmd = js"""$what.addClass(${classes.mkString("\"", " ", "\"")})"""
    def addClass(classes: Class*): JsCmd = addClass(classes.map(_.value.toString): _*)
    def removeJQ = js"""$what.remove()"""
    def removeClass[X: ClassTag](classes: String*): JsCmd = js"""$what.removeClass(${classes.mkString("\"", " ", "\"")})"""
    def removeClass(classes: Class*): JsCmd = removeClass(classes.map(_.value.toString): _*)
    def swapOut(content: NodeSeq, callBack: Option[JsCmd] = None): JsCmd = {
      val self = this
      val z: JsCmd = callBack.map(self.fadeIn(_)).getOrElse(self.fadeIn)
      self.fadeOut {
        self.empty &
          self.appendXml(content) & z
      }
    }

  }
  def stringToJQueryId(str: String) = s"""$$("#${str}")"""
  implicit def IdToJquery(id: Id) = new JQuery(stringToJQueryId(id.value.text))
  implicit def ClassToJquery(id: Class) = new JQuery(s"""$$(".${id.value.text}")""")

}
