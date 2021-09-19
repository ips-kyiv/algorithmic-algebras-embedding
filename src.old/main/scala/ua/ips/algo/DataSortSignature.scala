package ua.ips.algo

import scala.quoted._

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


trait DataSortSignatureRep1[T]:

   def name: String






