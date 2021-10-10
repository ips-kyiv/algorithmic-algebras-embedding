package ua.ips.algo.translation.languages.scala

import ua.ips.algo._
import ua.ips.algo.translation._

import scala.quoted._
import scala.quoted.staging._

//class ScalaLanguage extends Language {
class ScalaLanguage(using val qctx: Quotes) extends Language with ScalaGen {

  import qctx.reflect._

  type AstDef = Term


  val baseInterpretation: ua.ips.algo.Interpretation = ScalaInterpreter(ScalaInterperer.defaultSchemas)(using qctx)

  def constantDef(item: baseInterpretation.DataItem, sort: DataSort): AstDef = ???
      
  def dataSortDef(dataSort: DataSort): AstDef = ???

  def run(bundle: OutputBundle): baseInterpretation.DataItem = ???

  def signatureDef(items: Seq[AstDef]): AstDef = ???

  def write(bundle:    OutputBundle, dataDir: String): Unit = ???


}
