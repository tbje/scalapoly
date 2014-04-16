package tbje.web.util

import java.io.File
import net.liftweb.common.Box.box2Option
import net.liftweb.http.S
import net.liftweb.util.Props
import akka.actor.ActorSystem

object Config {
  val dynLoadComet = false
  val name = "ZermEx"
  val tagline = "A Swiss bitcoin exchange"
  val mainUrl = "https://zermex.com"
  val emailInfo = "info@zermex.com"

  val bootstrapVerison = "3.0.3"
  val jqueryVersion = "1.9.1"

  val gmailUsername = Props.get("gmail.username").getOrElse("")
  val gmailPassword = Props.get("gmail.password").getOrElse("")
  val supportedLocales = "en_GB" :: "nb_NO" :: Nil
  var storageLocation: File = new File("/")
  var forceInsecure = false
  var buildNumber: Option[String] = None
  def ip: Option[String] = S.getRequestHeader("X-Real-IP").or(S.request.map(_.remoteAddr))
  val serverId = Option(System.getProperty("node.name"))
  def prependServerId(code: String) = Config.serverId.getOrElse("000") + code

  private lazy val testHost = {
    import java.net._
    import collection.JavaConverters._
    val addressesInterfaces =
      NetworkInterface.getNetworkInterfaces.asScala.toList.flatMap {
        _.getInetAddresses.asScala.toList.filter(_.isSiteLocalAddress).headOption
      }
    val addresses = addressesInterfaces.map(_.getAddress.mkString("."))
    val ip = addresses.filterNot(_.contains("-")).headOption.getOrElse("localhost")
    s"http://$ip:8080"
  }

  val system = ActorSystem(name)

}
