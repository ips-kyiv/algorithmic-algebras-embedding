package ua.ips.algo.langtools

import scala.compiletime._
import scala.deriving._
import scala.quoted._


trait Traversable[T]:

  def traverse(t:T)(f: Any => Boolean): Boolean

  def processChilds(t:T)(f: Any => Boolean): Boolean



object  Traversable:


  inline def summonAll[T <: Tuple]: List[Traversable[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (head *: tail) => summonInline[Traversable[head]]::summonAll[tail]

  inline given derived[T](using m: Mirror.Of[T]): Traversable[T] =
     val elemInstances = summonAll[m.MirroredElemTypes]
     inline m match
       case s: Mirror.SumOf[T] => traversableSum(s, elemInstances)
       case p: Mirror.ProductOf[T] => traversableProduct(p, elemInstances)


  def traversableSum[T](s: Mirror.SumOf[T], elems: List[Traversable[_]]) =
    new Traversable[T]:
      
      def traverse(t:T)(f: Any => Boolean): Boolean =
        elems(s.ordinal(t)).asInstanceOf[Traversable[Any]].traverse(t)(f)
    
      def processChilds(t:T)(f: Any => Boolean): Boolean =
        elems(s.ordinal(t)).asInstanceOf[Traversable[Any]].processChilds(t)(f)


  def traversableProduct[T](p: Mirror.ProductOf[T], elems: =>List[Traversable[_]]) =
    new Traversable[T]:

      def traverse(t:T)(f: Any => Boolean): Boolean =
        if (f(t))
          processChilds(t)(f)
        else
          false

      def productIterator[T](p: T) = p.asInstanceOf[Product].productIterator     

      def processChilds(t:T)(f: Any => Boolean): Boolean =
        productIterator(t).zip(elems).forall{case (c,tr)=>tr.asInstanceOf[Traversable[Any]].traverse(c)(f)}


      

   

