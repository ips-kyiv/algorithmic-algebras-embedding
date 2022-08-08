package ua.ips.algo.translation.languages.scala


import scala.quoted.*

import ua.ips.algo.{*,given}

class ScalaInterpreter(knownSchemas: Map[TypesOnlyDataSortSignature, SchemaRepresentation])(using qctx: Quotes) extends Interpretation:

  type DataItem = Matchable

  type DataScope = DummyDataScope.type

  def newDataScope(): DataScope = DummyDataScope

  def applyInternal(scope: DataScope, signature: TypesOnlyDataSortSignature, args: Seq[DataItem]): DataItem = 
    knownSchemas.get(signature) match
      case Some(repr) =>
        repr.apply(using this)(scope, args)
      case None =>
        throw new InterpretationException(s"Unknonw scema with signature: ${signature}")

  def constant[T](scope: DataScope, value: T, rep: DataSortRep[T]): DataItem = value

  def javaRefConstant(scope: DataScope, value: AnyRef, rep: DataSortRep[?]): DataItem = value


  // TODO: check, mb we b
  def extract[T](scope: DataScope, item: DataItem, rep: DataSortRep[T]): Option[T] =
    item match
      case t:T => Some(t)
      case _  => None


object ScalaInterperer{

   given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)
   
   val IntDataSort = IntBasicRep.dataSort

   val defaultSchemas = Map(
     DataSortSignature(Seq("base","int"),"+", Seq(("a",IntDataSort),("b",IntDataSort)),IntDataSort).typesOnly -> 
                                  Fun2SchemaRepresentation("+",(a:Int, b:Int) => a + b, 
                                  '{ (a:Int, b:Int) => a + b } ),
     DataSortSignature(Seq("base","int"),"-", Seq(("a",IntDataSort),("b",IntDataSort)),IntDataSort).typesOnly -> 
                                  Fun2SchemaRepresentation("-",(a:Int, b:Int) => a - a, 
                                  '{ (a:Int, b:Int) => a + b } ),

   )

}