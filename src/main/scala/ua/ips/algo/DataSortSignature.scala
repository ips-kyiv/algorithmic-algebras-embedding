package ua.ips.algo

import scala.quoted._

/**
 * Signature without argument names (only types)
 **/
case class TypesOnlyDataSortSignature(packageName: Seq[String], name: String, in: Seq[DataSort], out: DataSort)

case class DataSortSignature(
             packageName: Seq[String],
             name: String,   
             in: Seq[(String,DataSort)],
             out: DataSort
           ):

  def lift(using Quotes):Expr[DataSortSignature] =
       '{DataSortSignature(${Expr.ofSeq(packageName.map(x => Expr(x)))},  ${Expr(name)}, 
               ${Expr.ofSeq(in.map(x => 
                   Expr.ofTupleFromSeq(Seq(Expr(x._1),x._2.lift)).asExprOf[(String,DataSort)]
                 ))},
               ${out.lift})
        }

  lazy val typesOnly: TypesOnlyDataSortSignature = TypesOnlyDataSortSignature(packageName, name, in.map(_._2), out)


trait DataSortSignatureRep1[T]:

   def name: String






