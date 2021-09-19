package ua.ips.algo.translation

import ua.ips.algo._


trait Language {

  type AstDef

  case class OutputBundle(name: String, compilationUnits:Map[String,AstDef])

  def genContext(ctx: IRContext): OutputBundle;

  val baseInterpretation: Interpretation;

  def dataSortDef(dataSort: DataSort): AstDef

  def constantDef(item: baseInterpretation.DataItem, sort: DataSort): AstDef

  def signatureDef(items: Seq[AstDef]): AstDef
  
  def run(bundle: OutputBundle): baseInterpretation.DataItem

  def write(bundle: OutputBundle, dataDir: String): Unit

}
