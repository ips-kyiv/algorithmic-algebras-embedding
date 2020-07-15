package ua.ips.algo.languages.scala

import ua.ips.algo._

import scala.quoted._
//import scala.quoted.staging._

//class ScalaLanguage extends Language {
trait ScalaLanguage(using val qctx: QuoteContext) extends Language {

  import qctx.tasty._

  type Ast = Term

}
