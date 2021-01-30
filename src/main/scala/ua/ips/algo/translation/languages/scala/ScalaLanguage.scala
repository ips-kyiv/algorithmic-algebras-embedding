package ua.ips.algo.translation.languages.scala

import ua.ips.algo._
import ua.ips.algo.translation._

import scala.quoted._
//import scala.quoted.staging._

//class ScalaLanguage extends Language {
trait ScalaLanguage(using val qctx: Quotes) extends Language {

  import qctx.reflect._

  type Ast = Term

  

}
