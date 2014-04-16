package tbje.web.util

import scala.util.Random
import scala.util.control.Exception._

object UniqueRandomKey {
  /**
   * Creates an unique random key.
   * Ex:
   * val chars = ('a' to 'z') ++ ('1' to '9')
   * val key = uniqueRandomKey(chars.mkString, 22, (x:String)=>!keyExists?(x))
   */
  val alphaNumChars = ('a' to 'z') ++ ('A' to 'Z') ++ ('1' to '9') mkString
  def apply(uniqueFunc: String => Boolean, retries: Int = 10, chars: String = alphaNumChars, length: Int = 22): Option[String] = {
    val newKey = (1 to length).map(x => chars(Random.nextInt(chars.length))).mkString
    if (uniqueFunc(newKey)) Some(newKey) else { if (retries > 0) apply(uniqueFunc, retries - 1, chars, length) else None }
  }

  def exception[E <: Throwable](uniqueFunc: String => Unit, retries: Int = 10, chars: String = alphaNumChars, length: Int = 22)(implicit m: Manifest[E]): Option[String] = {
    apply(key => catching(m.runtimeClass) opt { uniqueFunc(key) } isDefined, retries, chars, length)
  }
}

