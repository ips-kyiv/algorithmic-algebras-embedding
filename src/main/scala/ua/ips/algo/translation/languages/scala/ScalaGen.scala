package ua.ips.algo.translation.languages.scala

import scala.quoted.*
import ua.ips.algo.translation.*
import ua.ips.algo.{*,given}

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

  def genMainNode(irCtx: IRContext, name: String): AstDef = {
     val ctx = ScalaGenContext(irCtx)
     val mainDecl = genMainDeclaration(ctx, name) 
     /*
     val mainDecls: List[DefDef] = List(mainDecl)
     val defintions: List[DefDef] = ctx.functionDefinitions.toList

     TranslationUnit(
        mainDecls ++ defintions  
     )
     */
     mainDecl
  }

  def genMainDeclaration(ctx: ScalaGenContext, name: String): AstDef = {
      import qctx.reflect.*
      //val inputs = ctx.generateInputs()
      //val inputs = ctx.generateInputs(ctx.irCtx.rootNode);
      val expr = '{
        new Fun1SchemaRepresentation[Int,Int](${Expr(name)}, x => x+1, '{x => x+1})
      }
      expr.asTerm
  }

}

