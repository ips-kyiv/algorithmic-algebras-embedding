package ua.ips.algo

import scala.quoted._

case class DataSortSignature(
             name: String,   // TODO: think about module system
             in: Seq[(String,DataSort)],
             out: DataSort
           )

trait DataSortSignatureRep[T] {

  def build(t:Expr[T]): DataSortSignature

}


