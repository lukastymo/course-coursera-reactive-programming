package week1

import week1.JSON.{JSeq, JNum, JStr, JObj}

object JSON {

  sealed trait JSON
  case class JSeq(elems: List[JSON]) extends JSON
  case class JObj(bindings: Map[String, JSON]) extends JSON
  case class JNum(num: Double) extends JSON
  case class JStr(str: String) extends JSON
  case class JBool(b: Boolean) extends JSON
  case object JNull extends JSON

  def show(json: JSON): String = json match {
    case JSeq(elems) =>
      s"[${elems map show mkString ", "}]"
    case JObj(bindings) =>
      val str = bindings map { case (key, value) =>
          s""""$key": ${show(value)}"""
      } mkString ", "
      s"{$str}"
    case JNum(num) => num.toString
    case JStr(str) => s""""$str""""
    case JBool(b) => b.toString
    case JNull => "null"
  }
}

object JsonApp extends App {
  import JSON._

  val data = JObj(Map(
    "firstName" -> JStr("John"),
    "lastName" -> JStr("Smith"),
    "address" -> JObj(Map(
      "streetAddress" -> JStr("1 2nd Street"),
      "postalCode" -> JNum(10023)
    )),
    "phoneNumbers" -> JSeq(List(
      JObj(Map("type" -> JStr("home"), "number" -> JStr("111 222 333"))),
      JObj(Map("type" -> JStr("work"), "number" -> JStr("444 555 666")))
    ))
  ))

  println(show(data).toString)
}
