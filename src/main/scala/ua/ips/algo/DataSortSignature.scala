package ua.ips.algo

import scala.quoted._

/**
 * Signature without argument names (only types)
 **/
case class TypesOnlyDataSortSignature(name: String, in: Seq[DataSort], out: DataSort)

case class DataSortSignature(
             name: String,   // TODO: think about module system
             in: Seq[(String,DataSort)],
             out: DataSort
           ):

  def lift(using Quotes):Expr[DataSortSignature] =
       '{DataSortSignature(${Expr(name)}, 
               ${Expr.ofSeq(in.map(x => 
                   Expr.ofTupleFromSeq(Seq(Expr(x._1),x._2.lift)).asExprOf[(String,DataSort)]
                 ))},
               ${out.lift})
        }

  lazy val typesOnly: TypesOnlyDataSortSignature = TypesOnlyDataSortSignature(name, in.map(_._2), out)


trait DataSortSignatureRep1[T]:

   def name: String






