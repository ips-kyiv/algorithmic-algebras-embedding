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
 

  def tryBuild[X:quoted.Type](f:Expr[X]): Expr[Schema] = 
     try
       val r = buildImpl[X](f)
       println(s"schema-expression: $r")
       r
     catch
       case SchemaCompileException(msg,posExpr) =>
          report.error(msg, posExpr)  
          '{???}

  def buildImpl[X:quoted.Type](f:Expr[X]): Expr[Schema] = {
      f.asTerm match 
         case Lambda(params, body) =>
                 val inputs = params.map{ param =>
                     val paramSort = findDataSort(param.tpt.tpe, f)
                     '{ InputSchema.Entry(${Expr(param.name)}, ${paramSort}) } 
                 }
                 val bodySchema = processTerm(body, '{ SchemaBase(sorts=${Expr.ofSeq(inputs)}.map(_.sort).toSet, signatures=Set.empty) } )
                 val base = bodySchema.base
                 '{
                   SequentialSchema(
                    InputSchema(${Expr.ofSeq(inputs)}),
                    ${bodySchema.value}
                   )
                 }
         case Inlined(x,List(),body) => buildImpl[X](body.asExprOf[X])
         case Block(List(),last) => buildImpl[X](last.asExprOf[X])
         case _ =>
              throw SchemaCompileException(s"lambda function expected, we have ${f.asTerm}",f)
  }




  def findDataSort(tp: TypeRepr, posExpr: Expr[_]): Expr[DataSort] = 
    tp.widen.asType match
      case '[t] => Expr.summon[DataSortRep[t]] match
                      case Some(r) => '{ $r.dataSort }
                      case None => 
                           throw SchemaCompileException(s"Can't find DataSortRep for ${tp.show}", posExpr)
      case _ => throw SchemaCompileException("Can't determinate type for ${tp.seal.show}", posExpr)


  def processTerm(body: Term, base: Expr[SchemaBase]): WithBase[Schema] =
    body match
      case Block(statements,last) => processBlock(statements, last, base)
      case If(cond,ifTrue,ifFalse) => processIf(cond, ifTrue, ifFalse, base)
      case While(cond,body) => processWhile(cond, body, base)
      case id@Ident(name) => processIdent(id, base)
      case app@Apply(obj, args) => processApply(app, base)
      case lt@Literal(constant) => processLiteral(lt, base)
      case Assign(lhs,rhs) => processAssign(lhs, rhs, base)
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

  private def processIf(cond:Term, ifTrue: Term, ifFalse:Term, base: Expr[SchemaBase]): WithBase[Schema] = 
    val predicate = processPredicate(cond, base)
    val ifTrueSchema = processTerm(ifTrue, predicate.base)
    val ifFalseSchema = processTerm(ifFalse, ifTrueSchema.base)
    val r = '{
       ConditionalSchema(${predicate.value}, ${ifTrueSchema.value}, ${ifFalseSchema.value})
    }
    WithBase(ifFalseSchema.base, r)
 
  private def processWhile(cond:Term, body:Term, base: Expr[SchemaBase]): WithBase[Schema] = 
    val predicate = processPredicate(cond, base)
    val bodySchema = processTerm(body,predicate.base)
    val r = '{
        LoopSchema(${predicate.value}, ${bodySchema.value})
    }
    WithBase(bodySchema.base, r)

  private def processAssign(lhs:Term, rhs:Term, base: Expr[SchemaBase] ): WithBase[Schema] =
    lhs match
      case Ident(name) =>
         val rhsSchema = processTerm(rhs, base)
         val r = '{
            AssignSchema(${Expr(name)}, ${rhsSchema.value}.asDataExpression)
         }
         WithBase(rhsSchema.base, r)
      case _ =>
            // TODO: implement array access.
            // TODO:  think how to represent (?)
         throw SchemaCompileException("ident in left part is expected", lhs.asExpr)

  private def processPredicate(cond: Term, base: Expr[SchemaBase]): WithBase[Condition] =
    cond match
      case Apply(Select(obj,"&&"),List(arg)) =>
              val frs = processPredicate(obj, base)
              val snd = processPredicate(arg, frs.base)
              val r = '{ AndCondition( ${frs.value}, ${snd.value}) }
              WithBase(snd.base,r)
      case Apply(Select(obj,"||"),List(arg)) =>
              val frs = processPredicate(obj, base)
              val snd = processPredicate(arg, frs.base)
              val r = '{ OrCondition( ${frs.value}, ${snd.value}) }
              WithBase(snd.base,r)
      case Apply(Select(obj,"!"),List()) =>
              val frs = processPredicate(obj, base)
              val r = '{ NotCondition( ${frs.value}) }
              WithBase(frs.base,r)
      case _ => 
              val v = processTerm(cond, base)
              val r = '{
                 ${v.value} match
                     case OutputSchema(e) =>
                             BaseCondition(e)
                     case _ => 
                             throw SchemaBuildException("Expected data expression")
              }
              WithBase(v.base,r)
              

  private def processApply(applyTerm: Apply, base: Expr[SchemaBase]): WithBase[Schema] =
    applyTerm match
      case Apply(TypeApply(sel@Select(obj,method),typeArgs), args) =>
         if (method=="foreach" && obj.tpe <:< TypeRepr.of[ParRange] ) {
            processParRange(obj, args.head, base)
         } else {
           throw SchemaCompileException(s"type arguments are not supported (term=${applyTerm})", applyTerm.asExpr)
         }
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
    

  private def processStatement(st: Statement, base: Expr[SchemaBase]): WithBase[Schema] = 
        st match
           case Import(term,selectors) => ???
           //case Export(x) => ???
           case d: Definition =>
              d match
                 case ValDef(name,typeTree,optRhs) =>
                   optRhs match
                     case Some(rhs) =>
                        val rhsSchema = processTerm(rhs, base)
                        val rhsExpr = '{
                               ${rhsSchema.value} match
                                  case OutputSchema(expr) => expr 
                                  case _ => throw SchemaBuildException("AAA")
                        }
                        // TODO: add lisst of names and track ValDef to Name                  
                        val r = '{ AssignSchema(${Expr(name)}, ${rhsExpr}) }
                        WithBase(rhsSchema.base, r)
                     case None =>
                        throw SchemaCompileException(s"Var should have init value ${d}", d.asExpr )
                 case _ => 
                   throw SchemaCompileException(s"definition other then ValDef are not supported ${d}", d.asExpr)
           case t: Term => processTerm(t, base)


  private def processParRange( parRange: Term, foreachArg: Term, base: Expr[SchemaBase]): WithBase[Schema] =
      foreachArg match
         case Lambda(params, body) =>
            if (params.length > 1) {
               throw SchemaCompileException("parRagne foreach should have one argument", foreachArg.asExpr)
            }
            var param = params.head
            val paramSort = findDataSort(param.tpt.tpe, foreachArg.asExpr)
            val (start,finish, workSize) = parRange match
               case Apply(Apply(par,List(range)),List(workSize)) =>
                  range match
                     case Apply(Select(startRangeWrapped,"to"),List(endRange)) =>
                        startRangeWrapped match
                           case Apply(intWrapper,List(startRange)) =>
                              (startRange, endRange, workSize)
                           case _ =>
                              throw SchemaCompileException("Can't parse startRange", startRangeWrapped.asExpr)
                     case _ =>
                        throw SchemaCompileException("Can't parse range, shoud be in form (x to y)",range.asExpr)
               case _ =>
                  throw SchemaCompileException("Can't parse parRange, should be in form (x to y).par(z)",parRange.asExpr)
            println(s"!parRange=${parRange}")
            println(s"!start=${start}")
            println(s"!finish=${finish}")
            println(s"!workSize = ${workSize} ")
            val startSchema = processTerm(start,base)
            val finishSchema = processTerm(finish,startSchema.base)
            val workSizeSchema = processTerm(workSize, finishSchema.base)
            val bodySchema = processTerm(body, workSizeSchema.base)
            val r = '{
               ParallelIterationSchema(
                  ${Expr(param.name)},
                  ${startSchema.value}.asDataExpression,
                  ${finishSchema.value}.asDataExpression,
                  ${workSizeSchema.value}.asDataExpression,
                  ${bodySchema.value}
               )
            }
            WithBase(bodySchema.base, r)
         case Inlined(x,List(),body) => processParRange(parRange, body, base)
         case Block(List(), body) => processParRange(parRange, body, base)
         case _ =>
            throw SchemaCompileException("Lambda in parRange foreach is expected", foreachArg.asExpr)


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

   inline def build[X](inline f: X): Schema = ${
      buildImpl[X]('f)
   }

   def buildImpl[X:Type](f:Expr[X])(using Quotes):Expr[Schema] = {
      import quotes.reflect._
      val embedding = new SchemaEmbedding
      embedding.tryBuild(f)
   }



}


