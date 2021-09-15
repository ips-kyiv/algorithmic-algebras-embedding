package ua.ips.algo.translation.languages.scala

import ua.ips.algo.*
import ua.ips.algo.translation.*

trait ScalaGen {

  this: ScalaLanguage =>

  override def genContext(ctx: IRContext): OutputBundle = 
    {
      val name = generateName(ctx, ctx.fullName)
      val ast = genMainNode(ctx, name)

      println(s"ast: $ast" )
      OutputBundle("output",
        Map(
           s"${name}.scala" -> ast,
      ))
    }

  
  def generateName(ctx: IRContext, fullName: Seq[String]): String =
    fullName.mkString(".")

  def genMainNode(ctx: IRContext, name: String): AstDef = {
    ???
    /*
     val ctx = ScalaGenContext(irCtx)
     val mainDecl = genMainDeclaration(ctx, name) 
     val mainDecls: List[DefDef] = List(mainDecl)
     val defintions: List[DefDef] = ctx.functionDefinitions.toList

     TranslationUnit(
        mainDecls ++ defintions  
     )
     */
  }


}