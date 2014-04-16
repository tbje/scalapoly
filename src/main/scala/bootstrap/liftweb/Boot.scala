package bootstrap.liftweb

import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import java.net.URL
import java.util.Locale
import scala.xml.NodeSeq
import tbje.web.templates._

class Boot {
  val faceliftResolver: PartialFunction[(Locale, List[String]), Box[NodeSeq]] = {
    case (_, "index" :: Nil) => Some(Default.index)
  }

  LiftRules.externalTemplateResolver.default.set(() => (() => faceliftResolver))

  val siteMapEntries = Seq(Menu.i("Home") / "index" >> Hidden)

  LiftRules.setSiteMap(SiteMap(siteMapEntries: _*))

  LiftRules.dispatch.append(tbje.web.util.JsConsoleWs.matcher)

  LiftRules.htmlProperties.default.set((r: Req) => new Html5Properties(r.userAgent))

  LiftRules.early.append(_.setCharacterEncoding("UTF-8")) // Force the request to be UTF-8

  LiftRules.addToPackages("tbje.web")

  val log4jUrl = Option(System.getProperty("log4j.configuration"))

  log4jUrl foreach { url =>
    println(s"Using $url log4j config file")
    Logger.setup = Full(Log4j.withFile(new URL(url)))
  }
}
