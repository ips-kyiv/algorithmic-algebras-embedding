package ua.ips.algo.translation.languages.scala


import scala.quoted.*

import ua.ips.algo.*

class ScalaInterpretation(using qctx: Quotes) extends Interpretation:

  
  type DataItem = Matchable

  def apply(signature: DataSortSignature, args: Seq[DataItem]): DataItem = ???

  def constant[T](value: T, rep: DataSortRep[T]): DataItem = value

  // TODO: check, mb we b
  def extract[T](item: DataItem, rep: DataSortRep[T]): Option[T] =
    item match
      case t:T => Some(t)
      case _  => None
