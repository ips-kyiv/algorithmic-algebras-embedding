package ua.ips.algo

import scala.quoted._

class SchemaEmbedding(using val qctx: QuoteContext) {

  import qctx.tasty._

  case class SchemaPart[T](
      expr: Expr[T] ,
      sortTypes: Set[qctx.tasty.Type],
      names: Map[String, DataSort]
  )

  def buildImpl[A:quoted.Type, B:quoted.Type](f:Expr[A=>B]):Expr[Schema] = {
      f.unseal match 
         case Lambda(params, body) =>
                 val startPart = buildStartPart()
                 val param = params.head
                 val paramSort = findDataSort(param.tpt.tpe, startPart, f)
                 val bodySchema = processTerm(body, paramSort)
                 '{ 
                    SequentialSchema(
                     InputSchema(${Expr(param.name)}, ${paramSort.expr}),
                     ${bodySchema.expr}
                    )
                  }
         case _ =>
              report.error("lambda function expected", f)  
              '{???}
  }


  def buildStartPart() = SchemaPart[Unit](
                              expr = '{()},
                              sortTypes = Set.empty,
                              names = Map.empty
                            )


  def findDataSort(tp: qctx.tasty.Type, state: SchemaPart[_], posExpr: Expr[_]): SchemaPart[DataSort] = {

    def retrieveSummon[T:quoted.Type](t: quoted.Type[T]): Option[Expr[DataSortRep[T]]] =
       Expr.summon[DataSortRep[T]] 

    import qctx.tasty._
    tp.seal match
      case '[$t] => retrieveSummon(t) match
                      case Some(r) => 
                        SchemaPart('{ ${r}.dataSort }, sortTypes = state.sortTypes + tp, names=state.names)
                      case None => report.error("Can't find DataSortRep for ${t.show}", posExpr)
                                   SchemaPart('{???}, state.sortTypes, state.names)
      case _ => report.error("Can't determinate type for ${tp.seal.show}")
                ??? 

  }

  def processTerm(body: Term, state: SchemaPart[_]): SchemaPart[Schema] =
    body match
      case Block(statements,last) => processBlock(statements, last, state)
      case app@Apply(obj, args) => processApply(app, state)
      case Literal(constant) => processLiteral(constant, state)
      case _ => 
           report.error("term is not supported yet ${body}", body.seal)
           ???

  private def processBlock(statements:List[Statement], last: Term, state: SchemaPart[_]): SchemaPart[Schema] = 
    statements match
      case Nil => processTerm(last, state)
      case head::tail => 
         val frs = processStatement(head, state)
         val snd = processBlock(tail, last, frs)
         val expr = '{  SequentialSchema( ${frs.expr}, ${snd.expr} ) }
         SchemaPart(expr, snd.sortTypes, snd.names)

  private def processApply(applyTerm: Apply, state: SchemaPart[_]): SchemaPart[Schema] =
    applyTerm match
      case Apply(Select(obj,method),args) =>
         ???
      case Apply(Ident(name),args) =>
         ???
    
  private def processStatement(st: Statement, state: SchemaPart[_]): SchemaPart[Schema] = ???

  
  private def processLiteral(term: Constant, state: SchemaPart[_]): SchemaPart[Schema] = ???

}

object SchemaEmbedding {

  inline def build[A,B](inline f: A=>B): Schema = ${
      buildImpl[A,B]('f)
  }


  def buildImpl[A:Type, B:Type](f:Expr[A=>B])(using qctx:QuoteContext):Expr[Schema] = {
      import qctx.tasty._
      val embedding = new SchemaEmbedding
      embedding.buildImpl(f)
  }

}


