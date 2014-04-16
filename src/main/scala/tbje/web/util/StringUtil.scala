package tbje.web.util

object StringUtil {
  def camelCase(x: String) =
    x.foldLeft((false, "")) {
      case ((false, soFar), '_') => (true, soFar)
      case ((true, soFar), '_') => (true, soFar)
      case ((false, soFar), c) => (false, soFar + c)
      case ((true, soFar), c) => (false, soFar + c.toUpper)
    }._2
}
