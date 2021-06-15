package ua.ips.algo.translation.languages.gpuc


import ua.ips.algo._
import ua.ips.algo.translation._
import java.io._
import java.nio.file._


trait CBase extends Language {

  type AstDef = TranslationUnit

  def genContext(ctx: IRContext): OutputBundle = 
    {
      val name = "Main.c"
      val ast = genMainNode(ctx)
      OutputBundle("output",Map(name -> ast))
    }
  
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
    

  def genMainNode(ctx: IRContext): TranslationUnit = {
     TranslationUnit(genStandardDeclarations(ctx) ++ List(genMainDeclaration(ctx)))
  }

  def genStandardDeclarations(ctx: IRContext): List[ExternalDeclaration] = {
    List.empty
  }

  def genMainDeclaration(ctx: IRContext): ExternalDeclaration = {
     FunctionDefinition(specifiers=List.empty, //: List[DeclarationSpecifier], 
            declarator = Declarator(None, 
              FunctionDirectDeclarator(
                Identifier("main"),
                ParameterTypeList(List.empty, false)  // TODO:  generate params for char** args
              )
            ), 
            declarations = List.empty, //: List[Declaration],  TODO: vars
            body = genCompoundStatement(ctx, ctx.rootNode)
      ) 
  }

  def genCompoundStatement(ctx: IRContext, node: IRNode): CompoundStatement = {
    node match
      case SeqIRNode(id,schema,childs) =>
        ???
      case _ => ???

  }





  

}
