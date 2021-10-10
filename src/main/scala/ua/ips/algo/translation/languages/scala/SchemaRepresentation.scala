package ua.ips.algo.translation.languages.scala

import ua.ips.algo.*
import scala.quoted.*

abstract class SchemaRepresentation {

  def name: String

  def apply(using interpreter: ScalaInterpreter)(args: Seq[interpreter.DataItem]): interpreter.DataItem

  protected def extractOrThrow[A](using interpreter: ScalaInterpreter)(item: interpreter.DataItem, rep: DataSortRep[A]): A =
    rep.extractItem(interpreter)(item) match
      case Some(v) => v
      case None =>
        throw new InterpretationException(s"can't extract ${rep.dataSort} from ${item}");

}


class Fun1SchemaRepresentation1[A:DataSortRep,B:DataSortRep](val name: String, fun: A=>B, exprFun: Quotes ?=> Expr[A=>B]) extends SchemaRepresentation {

  
  def apply(using interpreter: ScalaInterpreter)(args: Seq[interpreter.DataItem]): interpreter.DataItem = {
    val nArgs = args.length;
    if (nArgs == 1) {
      val a = extractOrThrow(args(0),summon[DataSortRep[A]])
      val b = fun(a)
      summon[DataSortRep[B]].constant(interpreter)(b)
    } else {
      throw new InterpretationException(s"function ${name} should have one argument")
    }
  }


}

class Fun2SchemaRepresentation1[A1:DataSortRep, A2:DataSortRep, B:DataSortRep](val name: String, fun: (A1,A2)=>B, 
                                exprFun: Quotes ?=> Expr[(A1,A2)=>B]) extends SchemaRepresentation {

  
  def apply(using interpreter: ScalaInterpreter)(args: Seq[interpreter.DataItem]): interpreter.DataItem = {
    val nArgs = args.length;
    if (nArgs == 2) {
      var c = args
      val a1 = extractOrThrow(c.head, summon[DataSortRep[A1]])
      c = c.tail
      val a2 = extractOrThrow(c.head, summon[DataSortRep[A2]])
      val b = fun(a1,a2)
      summon[DataSortRep[B]].constant(interpreter)(b)
    } else {
      throw new InterpretationException("function should have two elements")
    }
  }

}


class Fun3SchemaRepresentation1[A1:DataSortRep, A2:DataSortRep, A3: DataSortRep, B:DataSortRep](
                val name: String, 
                fun: (A1,A2,A3)=>B, 
                exprFun: Quotes ?=> Expr[(A1,A2,A3)=>B]) extends SchemaRepresentation {

  
  def apply(using interpreter: ScalaInterpreter)(args: Seq[interpreter.DataItem]): interpreter.DataItem = {
    val nArgs = args.length;
    if (nArgs == 3) {
      var c = args
      val a1 = extractOrThrow(c.head, summon[DataSortRep[A1]])
      c = c.tail
      val a2 = extractOrThrow(c.head, summon[DataSortRep[A2]])
      c = c.tail
      val a3 = extractOrThrow(c.head, summon[DataSortRep[A3]])
      val b = fun(a1,a2,a3)
      summon[DataSortRep[B]].constant(interpreter)(b)
    } else {
      throw new InterpretationException("function should have 3 arguments")
    }
  }

}
