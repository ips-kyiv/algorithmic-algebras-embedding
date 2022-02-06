package ua.ips.algo

import scala.quoted._

/**
 * SourcrPositon usef for error reporting.
 **/
case class SourcePosition(
  file: String,
  startLine: Int,
  startOffset: Int,
  endOffset: Int
) {

  def lift(using Quotes): Expr[SourcePosition] = '{
    SourcePosition(${Expr(file)}, ${Expr(startLine)}, ${Expr(startOffset)}, ${Expr(endOffset)})
  }

}

object SourcePosition {

   def treePos(using Quotes)(tree: quotes.reflect.Tree): SourcePosition = {
     quotePos(tree.pos)
   }

   def quotePos(using Quotes)(pos: quotes.reflect.Position): SourcePosition = {
    SourcePosition(
      pos.sourceFile.name,
      pos.startLine,
      pos.start,
      pos.end
    )
   }

   inline def internal:SourcePosition = ${
     internalImpl
   }

   def internalImpl(using Quotes): Expr[SourcePosition] = {
     import quotes.reflect.*
     quotePos(Position.ofMacroExpansion).lift
   }

}