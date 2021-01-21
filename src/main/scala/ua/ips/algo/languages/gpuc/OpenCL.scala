package ua.ips.algo.languages.gpuc


import ua.ips.algo._
import java.io._
import java.nio.file._


object OpenCL extends Language {

  type AstDef = TranslationUnit

  // yet not implemented
  val baseInterpretation: Interpretation = FreeInterpretation 

  def dataSortDef(dataSort: DataSort): AstDef = ???

  def constantDef(item: baseInterpretation.DataItem): AstDef = ???

  def signatureDef(items: Seq[AstDef]): AstDef = ???
  
  //
  //def predicateDef(): Seq[AstDef]


  def run(bundle: OutputBundle): baseInterpretation.DataItem = ???


  def write(bundle: OutputBundle, dataDir: String): Unit =
    val path = Path.of(dataDir)
    for((name,ast) <- bundle.compilationUnits) {
      val file = outputFile(path, name)
      try {
        val startState = PrintState.create()
        val finishState = ast.print(startState)
        val s = finishState.collectBlock().value
        file.print(s);
      } finally {
        file.close()
      }
    }

  def outputFile(dataDir: Path, fname: String): PrintWriter = 
    val nPath = dataDir.resolve(fname);
    val bf = Files.newBufferedWriter(nPath,StandardOpenOption.CREATE)
    new PrintWriter(bf)
    

}
