package ua.ips.algo.translation.languages.scala


import scala.quoted.*

import ua.ips.algo.{*,given}

class ScalaInterpreter(knownSchemas: Map[TypesOnlyDataSortSignature, SchemaRepresentation])(using qctx: Quotes) extends Interpretation:

  type DataItem = Matchable

  def apply(signature: TypesOnlyDataSortSignature, args: Seq[DataItem]): DataItem = 
    knownSchemas.get(signature) match
      case Some(repr) =>
        repr.apply(using this)(args)
      case None =>
        throw new InterpretationException(s"Unknonw scema with signature: ${signature}")

  def constant[T](value: T, rep: DataSortRep[T]): DataItem = value

  // TODO: check, mb we b
  def extract[T](item: DataItem, rep: DataSortRep[T]): Option[T] =
    item match
      case t:T => Some(t)
      case _  => None


object ScalaInterperer{

   given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)
   
   val IntDataSort = IntBasicRep.dataSort

   val defaultSchemas = Map(
     DataSortSignature("+", Seq(("a",IntDataSort),("b",IntDataSort)),IntDataSort).typesOnly -> 
                                  Fun2SchemaRepresentation1("+",(a:Int, b:Int) => a + a, 
                                  '{ (a:Int, b:Int) => a + b } )
   )

}