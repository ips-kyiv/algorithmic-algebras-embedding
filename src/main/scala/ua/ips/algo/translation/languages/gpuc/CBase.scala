package ua.ips.algo.translation.languages.gpuc


import ua.ips.algo._
import ua.ips.algo.translation._
import java.io._
import java.nio.file._


trait CBase  {


  def outputFile(dataDir: Path, fname: String): PrintWriter = 
    ???
    /*
    val nPath = dataDir.resolve(fname);
    val bf = Files.newBufferedWriter(nPath,StandardOpenOption.CREATE,StandardOpenOption.TRUNCATE_EXISTING)
    new PrintWriter(bf)
    */
    
  def generateName(fullName: Seq[String]): String =
    ???
    // for now - le't d
    //fullName.mkString

  def genMainNode(irCtx: IRContext, name:String): TranslationUnit = 
    ???
  


  def genMainDeclaration(ctx: CBaseGenContext, name: String): ExternalDeclaration = {
     ???
  }

  def genCompoundStatement(ctx: CBaseGenContext, node: IRNode): CompoundStatement = {
    ???
  }

  def genIVarSpecifiers(ctx: CBaseGenContext, v: IRVar): List[SpecifierQualifier] =
      genTypeSpecifiers(ctx, v.init.origin.sort)
      


  /**
   * create in context definition and prototype and generate function call submitted.
   **/
  def genParFunctionSubmit(ctx: CBaseGenContext, node: IRNode, barrierName: String): List[Statement] = {
    println(s"genParFunctioNSubmit is not implemented for ${node}")
    ???
  }

  def genBarrierInit(id: String, ctx: CBaseGenContext, count: Int): (Declaration, String) =
    ???

  def genBarrierAwaitCall(barrierName: String, ctx: CBaseGenContext): Statement =
    ???

  def genBlockItems(ctx: CBaseGenContext, node: IRNode): List[BlockItem] =
    ???
  
  def genVariableDeclaration(ctx: CBaseGenContext, name: String, init: DataExpression): Declaration = 
    ???

  def genBasicVariableDeclaration(ctx: CBaseGenContext, name: String, 
                                   sort: BasicDataSort, init: DataExpression): Declaration =
      ???
      

  def genArrayVariableDeclaration(ctx: CBaseGenContext, name: String,
                                  sort: FixedArrayDataSort, init: DataExpression): Declaration = ???


  def genTypeSpecifiers(ctx: CBaseGenContext, sort: DataSort): List[SpecifierQualifier] =
    ???

  def genAssigment(ctx: CBaseGenContext, name: String, expr: DataExpression): AssigmentExpression = 
    ???

  def genExpression(ctx: CBaseGenContext, expr: DataExpression): Expression = 
    ???

  def genPrecAssigmentExpression(ctx: CBaseGenContext, expr: DataExpression): PrecAssigmentExpression =
    ???

  def isSpecialSignature(signature: DataSortSignature): Boolean =
    ???

  def SpecialFunctionalExpression(signature: DataSortSignature, args: List[PrecAssigmentExpression]):PrecAssigmentExpression =
    ???


  def toMultiplicativeExpression(expr: Expression): MultiplicativeExpression =
    ???

  def toCastExpression(expr: Expression): CastExpression =
    expr match
      case cExpr: CastExpression => cExpr
      case _ => WrappedExpression(expr)


  def generateConstantExpression(vtx: CBaseGenContext, sort: DataSort, value: String): Expression =
    ???

}
