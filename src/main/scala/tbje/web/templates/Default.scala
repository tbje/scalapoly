package tbje.web.templates

import scala.xml.NodeSeq
import tbje.web.util.HtmlUtil._
import tbje.web.util.Config
import scala.xml._
import tbje.facelift.imports._
import tbje.facelift.html.Glyphicon
import tbje.facelift.html.Entities._
import tbje.web.comet.{ JsConsoleComet }
import scala.reflect._
import scala.reflect.runtime.universe._
import net.liftweb.http.CometActor
import net.liftweb.http.js.JsCmds.Alert
import net.liftweb.http.js.JsCmd
import net.liftweb.http.SHtml

import tbje.facelift.css.{ BootstrapClasses => BS }

object Default {

  def mariginMinus = 620

  def banner =
    Div(Ids.bannerDiv, Style(MarginLeft(-29.px), PaddingLeft(30.px), MarginTop(82.px), PaddingTop(0.px))) {
      H1("Scalapoly")
    }

  def index =
    template(
      banner ++
        Div(BS.row, Style(MarginTop(10.px), PaddingLeft(30.px))) {
          H3("Start a new console based game") ++
            P("sbt > game") ++
            H3("Start a new game with web ui") ++
            P("sbt > webgame") ++
            P("For a better experience I recommend to start a new sbt session (Open new terminal, > cd scalapoly, > sbt) first. This way you avoid all the log messages from the web server.")
        })

  val buttons = Span(BS.srOnly)("Toggle navigation") ++ Span(BS.iconBar) ++ Span(BS.iconBar) ++ Span(BS.iconBar)

  def menu =
    Div(BS.navbar & BS.navbarDefault & BS.navbarFixedTop, Role("navigation"), Ids.navbarDivId) {
      Div(BS.container) {
        Div(BS.navbarHeader) {
          Button(Type.Button, BS.navbarToggle, DataToggle("collapse"), DataTarget(s"#${Ids.mainMenuCollapse.id}"))("")(buttons) ++
            A(BS.navbarBrand, Href("/")) {
              t"Scalapoly" ++ Span(BS.h6)(" - sponsored by ") ++ t"Zerm" ++ Span(Style(Color.hex("#ff8400")))("Ex")
            }
        } ++
          Div(Ids.dropDownField) { "" }
      }
    }

  val ieHack = Unparsed("""
                <!-- Le HTML5 shim, for IE6-8 support of HTML elements --> 
                  <!--[if lt IE 9]> 
                    <script src="/js/html5shim/3.7.0/html5.js"></script>
                  <![endif]-->""")

  import Config._

  def template(content: NodeSeq, title: String = s"${Config.name} - ${Config.tagline}") =
    Html {
      Head {
        Title(title) ++
          Meta(Charset.UTF8) ++
          Meta(Name.Description, Content("")) ++
          Meta(Name.Author, Content("")) ++
          Meta(Name.Viewport, Content("width=device-width, initial-scale=1.0")) ++
          Meta(Name.Viewport, Content("width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no")) ++
          ieHack ++
          Link(Href(s"/css/bootstrap/$bootstrapVerison/bootstrap.min.css"), Rel.Stylesheet)
      } ++
        Body {
          Div(LiftComet(classOf[JsConsoleComet])) ++
            menu ++
            Div(BS.container, Ids.contentDivId, Style(MarginBottom(100.px)))(content) ++
            Footer(BS.navbarDefault & BS.navbarFixedBottom) {
              Div(BS.container, Style(PaddingTop(20.px), PaddingBottom(0.px))) {
                A(Style(Color.Gray, Float.Right), Href("http://www.glyphicons.com"))("glyphicons") ++
                  P(Style(Color.Gray)) {
                    Copy ++ Nbsp ++ t"Trond Bjerkestrand" ++ t" - " ++ A(Href("https://twitter.com/tbjerkes"))("@tbjerkes") ++ t" - " ++ A(Href("https://github.com/tbje/scalapoly"))("Source code") 
                  }
              }
            } ++
            Script(Type.TextJavaScript, Src(s"/js/jquery/$jqueryVersion/jquery.min.js")) ++
            Script(Type.TextJavaScript, Src(s"/js/bootstrap/$bootstrapVerison/bootstrap.min.js"), Id("bootstrap"))
        }
    }
}
