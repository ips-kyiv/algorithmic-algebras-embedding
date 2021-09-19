package ua.ips.algo.translation.languages.gpuc


import ua.ips.algo._
import ua.ips.algo.translation._
import java.io._
import java.nio.file._


trait CBase  {


  def outputFile(dataDir: Path, fname: String): PrintWriter = 
    ???
    
  def generateName(fullName: Seq[String]): String =
    ???

  def genMainNode(irCtx: IRContext, name:String): TranslationUnit = 
    ???
  


  def toCastExpression(expr: Expression): CastExpression =
    expr match
      case cExpr: CastExpression => cExpr
      case _ => WrappedExpression(expr)


  //def generateConstantExpression(vtx: CBaseGenContext, sort: DataSort, value: String): Expression =
  //  ???

}
