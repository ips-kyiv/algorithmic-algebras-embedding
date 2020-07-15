package ua.ips.algo



trait Language {

  type AstDef

  case class OutputBundle(name: String, compilationUnits:Map[String,AstDef])


  val baseInterpretation: Interpretation;

  def dataSortDef(dataSort: DataSort): AstDef

  def constantDef(item: baseInterpretation.DataItem): AstDef

  def signatureDef(items: Seq[AstDef]): AstDef
  
  //
  //def predicateDef(): Seq[AstDef]


  def run(bundle: OutputBundle): baseInterpretation.DataItem


  def write(bundle: OutputBundle, dataDir: String): Unit

}
