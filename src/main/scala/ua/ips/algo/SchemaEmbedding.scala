package ua.ips.algo

import scala.quoted._
import scala.compiletime._

case class SchemaCompileException(msg:String, posExpr: Expr[Any]) extends RuntimeException

class SchemaEmbedding(using val qctx: Quotes) {

  import quotes.reflect._

  case class WithBase[T](
     base: Expr[SchemaBase] = '{ SchemaBase(sorts = Set.empty, signatures = Set.empty) },
     value: Expr[T]
  )
 

  def tryBuild[A:quoted.Type, B:quoted.Type](f:Expr[A=>B]): Expr[Schema] = 
     try
       val r = buildImpl[A,B](f)
       println(s"schema-expression: $r")
       r
     catch
       case SchemaCompileException(msg,posExpr) =>
          report.error(msg, posExpr)  
          '{???}

  def buildImpl[A:quoted.Type, B:quoted.Type](f:Expr[A=>B]): Expr[Schema] = {
      Term.of(f) match 
         case Lambda(params, body) =>
                 val param = params.head
                 val paramSort = findDataSort(param.tpt.tpe, f)
                 val bodySchema = processTerm(body, '{ SchemaBase(sorts=Set(${paramSort}), signatures=Set.empty) } )
                 val base = bodySchema.base
                 '{
                   SequentialSchema(
                    // TODO: more that one param
                    InputSchema(${Expr(param.name)}, ${paramSort}),
                    ${bodySchema.value}
                   )
                 }
         case Inlined(x,List(),body) => buildImpl[A,B](body.asExprOf[A=>B])
         case Block(List(),last) => buildImpl[A,B](last.asExprOf[A=>B])
         case _ =>
              throw SchemaCompileException(s"lambda function expected, we have ${Term.of(f)}",f)
  }




  def findDataSort(tp: TypeRepr, posExpr: Expr[_]): Expr[DataSort] = 
    tp.widen.asType match
      case '[t] => Expr.summon[DataSortRep[t]] match
                      case Some(r) => ' { $r.dataSort }
                      case None => 
                           throw SchemaCompileException(s"Can't find DataSortRep for ${tp.show}", posExpr)
      case _ => throw SchemaCompileException("Can't determinate type for ${tp.seal.show}", posExpr)


  def processTerm(body: Term, base: Expr[SchemaBase]): WithBase[Schema] =
    body match
      case Block(statements,last) => processBlock(statements, last, base)
      case id@Ident(name) => processIdent(id, base)
      case app@Apply(obj, args) => processApply(app, base)
      case lt@Literal(constant) => processLiteral(lt, base)
      case _ => 
           throw SchemaCompileException(s"term is not supported yet ${body}", body.asExpr)


  private def processBlock(statements:List[Statement], last: Term, base: Expr[SchemaBase]): WithBase[Schema] = 
    statements match
      case Nil => processTerm(last, base)
      case head::tail => 
         val frs = processStatement(head, base)
         val snd = processBlock(tail, last, frs.base)
         val expr =  '{ SequentialSchema( ${frs.value},  ${snd.value} ) }
         WithBase(base, expr)

  private def processApply(applyTerm: Apply, base: Expr[SchemaBase]): WithBase[Schema] =
    applyTerm match
      case Apply(TypeApply(sel@Select(obj,method),typeArgs), args) =>
         throw SchemaCompileException("type arguments are not supported", applyTerm.asExpr)
      case Apply(sel@Select(obj,method),args) =>
         val objSchema = processTerm(obj, base)
         val objSort = findDataSort(obj.tpe, obj.asExpr )
         var state = objSchema.base
         var argsExprs: List[Expr[DataExpression]] = Nil
         var cArgs = args
         while(!cArgs.isEmpty) {
             val cState = processTerm(cArgs.head,state)
             cArgs = cArgs.tail
             state = cState.base
             val expr = '{
                ${cState.value} match
                   case OutputSchema(e) => e
                   case _  =>
                      throw SchemaBuildException("Expected data expression")
             }
             argsExprs = expr::argsExprs
         }
         argsExprs = argsExprs.reverse
         // we assume, that object is a first argument
         val objExpr = '{
           ${objSchema.value} match
               case OutputSchema(e) => e
               case _ =>
                  throw SchemaBuildException("Expected data expression")
         }
         argsExprs = objExpr::argsExprs
         val outSort = findDataSort(applyTerm.tpe, applyTerm.asExpr)
         val argsSorts = argsExprs.map( r => '{ $r.sort } )
         val signature = buildDataSortSignature(sel, argsSorts, outSort)
         val newBase = '{  ${state}.copy(signatures = ${state}.signatures + ${signature})  }
         val newSchema = '{
             OutputSchema(FunctionalExpression(${signature},${Expr.ofSeq(argsExprs)}))
         }
         WithBase[Schema](base=newBase, value = newSchema)
      case Apply(Ident(name),args) =>
         ???
      case Apply(Apply(x,args1),args2) =>
         throw SchemaCompileException("curried arguments are not supported", applyTerm.asExpr)
      case _ =>
         throw SchemaCompileException("construction is not supported", applyTerm.asExpr)
    
  private def processStatement(st: Statement, base: Expr[SchemaBase]): WithBase[Schema] = ???

  private def processIdent(id: Ident, base: Expr[SchemaBase]): WithBase[Schema] =
        val sort = findDataSort(id.tpe.widen, id.asExpr)
        val newSchema = '{ OutputSchema(DataAccessExpression( ${Expr(id.name)}, ${sort} )) }
        WithBase(base, newSchema)
  
  private def processLiteral(term: Literal, state: Expr[SchemaBase]): WithBase[Schema] = 
        val sort = findDataSort(term.tpe.widen, term.asExpr)
        val newBase = '{  ${state}.copy(sorts = ${state}.sorts + ${sort})  }
        val schema = '{ OutputSchema(ConstantExpression(${sort},${Expr(term.constant.show)})) }
        WithBase(newBase, schema)


  //  TODO: check that appropriative operations on DataRep exists ?
  private def buildDataSortSignature(sel: Select, paramSorts: Seq[Expr[DataSort]], out: Expr[DataSort]): Expr[DataSortSignature] =
        val paramSymms = sel.symbol.paramSymss
        println(s"buildDataSignature, paramSymms=$paramSymms")
        if (paramSymms.isEmpty)
           throw SchemaCompileException("should be method call", sel.asExpr)
        if (paramSymms.size > 1)
           throw SchemaCompileException("not a simple function", sel.asExpr)
        val paramSyms = paramSymms.head
        val paramNames = paramSyms.map(_.name)
        val paramDescriptions: Seq[Expr[(String,DataSort)]] = paramNames.zip(paramSorts).map{
            case (name, sort) => Expr.ofTupleFromSeq(Seq(Expr(name),sort)).asExprOf[(String,DataSort)]
        }
        '{ DataSortSignature(${Expr(sel.name)}, ${Expr.ofSeq(paramDescriptions)}, $out) }
        
        
}

object SchemaEmbedding {

  inline def build[A,B](inline f: A=>B): Schema = ${
      buildImpl[A,B]('f)
  }

  def buildImpl[A:Type, B:Type](f:Expr[A=>B])(using Quotes):Expr[Schema] = {
      import quotes.reflect._
      val embedding = new SchemaEmbedding
      embedding.tryBuild(f)
  }

}


