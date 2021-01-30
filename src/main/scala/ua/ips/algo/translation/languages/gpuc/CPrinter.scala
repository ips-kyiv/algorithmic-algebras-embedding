package ua.ips.algo.translation.languages.gpuc

import scala.compiletime._
import scala.deriving._

case class OutputPosition(line: Int, col: Int)

case class PrintParams(
  leftMargin: Int, 
  rightMargin: Int, 
  ident: Int)

sealed trait PrintToken

case class PrintBlock(
  startCol: Int,
  nCols: Int,
  nLines: Int,
  value: String,
) extends PrintToken

case class SmallBlock(value: String) extends PrintToken


case object NewLine extends PrintToken 
case object Whitespace extends PrintToken
case object EmptyPrint extends PrintToken


case class PrintState(
   prevState: Option[PrintState],
   printed: IndexedSeq[PrintToken],
   pos: OutputPosition,
   params: PrintParams
)  {

  def addSmallBlock(value: String): PrintState =
    val len = value.length
    val nextPos = pos.col + value.length
    val block = SmallBlock(value)
    if (nextPos > params.rightMargin - params.leftMargin) then
      copy(printed=printed.appended(block).appended(NewLine),
           pos = pos.copy(line = pos.line+1, col=0)
      )
    else
      copy(printed = printed.appended(block), pos = pos.copy(col = pos.col + len))     


  def addNestedBlock(block: PrintBlock): PrintState =
    if block.nLines == 0 then
        addSmallBlock(block.value) 
    else
        copy(
          printed = printed.appended(block).appended(NewLine),
          pos = pos.copy(line = pos.line + block.nLines, col=0)
        )

  def addNewLine(): PrintState =
    copy(printed = printed.appended(NewLine),
         pos = pos.copy(line = pos.line+1, col=0))

  def addWhiteSpace(): PrintState =
    // TODO: add special logic
    addSmallBlock(" ")

  def add(token: PrintToken): PrintState =
    token match
      case b:PrintBlock => addNestedBlock(b)
      case s:SmallBlock => addSmallBlock(s.value)
      case NewLine => addNewLine()
      case Whitespace => addWhiteSpace()
      case EmptyPrint => this
 
  def print[T](t:T)(using Printer[T]): PrintState =
    summon[Printer[T]].print(t,this)    

  def startBlock(): PrintState =
    PrintState(
      prevState = Some(this),
      printed = IndexedSeq.empty,
      pos = OutputPosition(0,0),
      params = params.copy(leftMargin = params.leftMargin + params.ident)
    )

  def finishBlock(): PrintState =
    val block = collectBlock()    
    prevState match
      case Some(prev) =>
        prev.add(block)
      case None  =>
        // finish instead stast
        System.err.println("warn:  finish withot start")
        PrintState(None,IndexedSeq.empty,OutputPosition(0,0),params.copy(leftMargin=0)).add(block)

  
  def  collectBlock(): PrintBlock = 
    val sb = new StringBuilder()  
    var col = -params.leftMargin    
    val block0 = PrintBlock(
      startCol = params.leftMargin,
      0,0,""
    )
    val block1 = printed.foldLeft(block0){ (s,e) =>
      e match 
        case b:PrintBlock =>
          sb.append(b.value).append('\n')
          col = - params.leftMargin
          s.copy(nLines = s.nLines + b.nLines)
        case b:SmallBlock =>
          col = if (col < 0) {
            sb.append(" " * s.startCol)
            0
          } else {
            col
          }
          sb.append(b.value)
          col += b.value.length
          if (s.nCols < col) then
            s.copy(nCols = col)
          else
            s
        case Whitespace =>
          // TODO: merfe 
          sb.append(" ")
          col += 1
          s
        case NewLine =>
          sb.append("\n")
          col = -params.leftMargin
          s.copy(nLines = s.nLines + 1)
        case EmptyPrint =>
          // do nothing.
          s
    }
    if ( block1.nLines > 0 ) then
      block1.copy(value = sb.toString())
    else 
      block1.copy(nLines = 1, value=sb.append("\n").toString())
             
  def printedString():String =
    collectBlock().value

}

object PrintState {

   def create(): PrintState = 
     PrintState(None,IndexedSeq.empty, OutputPosition(0,0), PrintParams(0,132,2))
   

}


// TOOD:  implement pretty-printint instead
trait Printer[T]  {

  def print(data:T, state: PrintState): PrintState

}

object Printer {

  inline def summonAll[T <: Tuple]: List[Printer[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (head *: tail) => summonInline[Printer[head]]::summonAll[tail]

  inline given derived[T](using m: Mirror.SumOf[T]): Printer[T] =    
     val elemInstances = summonAll[m.MirroredElemTypes].toIndexedSeq
     new Printer[T]:
       def print(t:T, state: PrintState): PrintState =
          elemInstances(m.ordinal(t)).asInstanceOf[Printer[Any]].print(t,state)

}

extension [T](t:T)(using Printer[T])
   def print(state: PrintState): PrintState =
     summon[Printer[T]].print(t,state)

def constWordPrinter[T](word: String): Printer[T] = new Printer[T] {
     def print(data: T, state: PrintState): PrintState =
        state.addSmallBlock(word).addWhiteSpace()
}

case class SeparatedList[T,S](list: List[T], separator:S)

given separatedListPrinter[T,S](using Printer[T], Printer[S]): Printer[SeparatedList[T,S]] with 
  
      def print(data: SeparatedList[T,S], state: PrintState): PrintState =
        var isFirst = true
        var rest = data.list
        var s = state
        while(!rest.isEmpty) {
          val head = rest.head
          rest = rest.tail
          if (isFirst) {
            isFirst = false
          } else {
            s = s.print(data.separator)
          }
          s = s.print(head)
        }
        s


case class TrailingSeparatedList[T,S](list: List[T], separator:S)

given termListPrinter[T,S](using Printer[T], Printer[S]): Printer[TrailingSeparatedList[T,S]] with
  def print(data: TrailingSeparatedList[T,S], state: PrintState): PrintState =
    data.list.foldLeft(state)( (s,e) => s.print(e).print(data.separator) )


case class Tokens[T](values: T*)

given nonSeparatedPrinted[T](using Printer[T]): Printer[Tokens[T]] with
  def print(data: Tokens[T], state: PrintState): PrintState =
    data.values.foldLeft(state)( (s,e) => s.print(e) )


given Printer[PrintToken] with
    def print(data: PrintToken, state: PrintState) = state.add(data)      


case class InBlock[T]( nested: T, start: String, finish: String)

given nestedPrinter[T](using Printer[T]): Printer[InBlock[T]] with
  def print(data: InBlock[T], state: PrintState): PrintState =
    state.addSmallBlock(data.start)
         .startBlock()
         .print(data.nested)
         .finishBlock()
         .addSmallBlock(data.finish)
         .addNewLine()

 
given optionPrinter[T](using Printer[T]): Printer[Option[T]] with
  def print(data: Option[T], state: PrintState) =
    data match
      case Some(v) => state.print(v)
      case None => state

given Printer[TranslationUnit] with 

    def print(data: TranslationUnit, state: PrintState): PrintState =
      state.print(TrailingSeparatedList(data.declarations, NewLine))
 

given Printer[ExternalDeclaration] = Printer.derived

given Printer[Declaration] with 

    def print(data: Declaration, state: PrintState):PrintState =
      state.print(SeparatedList(data.specifiers, Whitespace))
           .print(SeparatedList(data.initDeclarators, Whitespace))
           .addSmallBlock(";").addNewLine()
      

given Printer[StaticAssert] with

    def print(data: StaticAssert, state: PrintState): PrintState =
       state.addSmallBlock("_Static_assert")
            .addSmallBlock("( ")
            .print(data.expression)
            .addSmallBlock(", ")
            .print(data.message)
            .addSmallBlock(")")

given Printer[FunctionDefinition] with

    def print(data: FunctionDefinition, state: PrintState) =
      state.print(data.specifiers)
           .addWhiteSpace()
           .print(data.declarator)
           .addWhiteSpace()
           .print(SeparatedList[Declaration,PrintToken](data.declarations, Whitespace))
           .print(data.body)

given straySemicolonPrinter: Printer[STRAY_SEMICOLON.type] with
    def print(data: STRAY_SEMICOLON.type, state: PrintState) =
      state.addSmallBlock(";")

// bug in dotty: https://github.com/lampepfl/dotty/issues/11177
//given Printer[DeclarationSpecifier] = Printer.derived
given Printer[DeclarationSpecifier] with

    def print(data: DeclarationSpecifier, state: PrintState) =
      data match
        case x:SpecifierQualifier => summon[Printer[SpecifierQualifier]].print(x,state)
        case x:StorageClassSpecifier => summon[Printer[StorageClassSpecifier]].print(x,state)
        case x:AddressSpaceQualifier => summon[Printer[AddressSpaceQualifier]].print(x,state)
        case x:FunctionSpecifier => summon[Printer[FunctionSpecifier]].print(x,state)
      

given Printer[List[DeclarationSpecifier]] with

    def print(data: List[DeclarationSpecifier], state: PrintState) =
       data.foldLeft(state)( (s,e) => s.print(e).addWhiteSpace())
      
given Printer[SpecifierQualifier] with

    def print(data: SpecifierQualifier, state: PrintState): PrintState =
      data match
        case x: TypeSpecifier => summon[Printer[TypeSpecifier]].print(x,state)
        case x: TypeQualifier => summon[Printer[TypeQualifier]].print(x,state)
        case x: AddressSpaceQualifier => summon[Printer[AddressSpaceQualifier]].print(x,state)

given Printer[TypeSpecifier] with

    def print(data: TypeSpecifier, state: PrintState): PrintState =
      data match
        case x:BaseTypeSpecifier => summon[Printer[BaseTypeSpecifier]].print(x,state)
        case x:AtomicTypeSpecifier => summon[Printer[AtomicTypeSpecifier]].print(x,state)
        case x:StructOrUnionSpecifier => summon[Printer[StructOrUnionSpecifier]].print(x,state)
        case x:EnumSpecifier => summon[Printer[EnumSpecifier]].print(x,state)
        case x:TypeDefName => summon[Printer[TypeDefName]].print(x,state)
        case x:PointerTypeSpecifier => summon[Printer[PointerTypeSpecifier]].print(x,state)
        

given Printer[TypeQualifier] = Printer.derived

given constPrinter: Printer[CONST.type] = constWordPrinter[CONST.type]("const")
given volatilePrinter: Printer[VOLATILE.type] = constWordPrinter[VOLATILE.type]("volatile")

given Printer[StorageClassSpecifier] = Printer.derived

given typedefPrinter: Printer[TYPEDEF.type] = constWordPrinter("typedef")
given externPrinter: Printer[EXTERN.type] = constWordPrinter("extern")
given staticPrinter: Printer[STATIC.type] = constWordPrinter("static")
given threadLocalPrinter: Printer[_THREAD_LOCAL.type] = constWordPrinter("_Thread_local")
given autoPrinter: Printer[AUTO.type] = constWordPrinter("auto")
given regPrinter: Printer[REGISTER.type] = constWordPrinter("register")

given Printer[AddressSpaceQualifier] = Printer.derived

given localPrinter: Printer[LOCAL.type] = constWordPrinter("local")
given globalPrinter: Printer[GLOBAL.type] = constWordPrinter("global")
given constantPrinter: Printer[CONSTANT.type] = constWordPrinter("constant")
given privatePrinter: Printer[PRIVATE.type] = constWordPrinter("private")
given genericPrinter: Printer[GENERIC.type] = constWordPrinter("generic")

given Printer[BaseTypeSpecifier] = Printer.derived

given printerVOID: Printer[VOID.type] = constWordPrinter("void") 
given printerCHAR: Printer[CHAR.type] = constWordPrinter("char")
given printerSHORT: Printer[SHORT.type] = constWordPrinter("short")
given printerINT:  Printer[INT.type] = constWordPrinter("int")
given printerLONG: Printer[LONG.type] = constWordPrinter("long")
given printerFLOAT: Printer[FLOAT.type] = constWordPrinter("float") 
given printerDOUBLE: Printer[DOUBLE.type] = constWordPrinter("double") 
given printerSIGNED: Printer[SIGNED.type] = constWordPrinter("signed")
given printerUNSIGNED: Printer[UNSIGNED.type] = constWordPrinter("unsigend")
given printer_BOOL: Printer[_BOOL.type] = constWordPrinter("_Bool")
given printer_COMPLEX: Printer[_COMPLEX.type] = constWordPrinter("_Complex")
given printer__M128: Printer[__M128.type] = constWordPrinter("__m128")
given printer__M128D: Printer[__M128D.type] = constWordPrinter("__m128d")
given printer__M128I: Printer[__M128I.type] = constWordPrinter("__m128i")



given Printer[AtomicTypeSpecifier] with
    def print(data: AtomicTypeSpecifier, state: PrintState): PrintState =
      state.addSmallBlock("_Atomic").addSmallBlock("(").print(data.typeName).addSmallBlock(")")


given Printer[FunctionSpecifier] with
    def print(data: FunctionSpecifier, state: PrintState): PrintState =
      data match
        case INLINE => printerINLINE.print(INLINE,state)
        case NORETURN => printerNORETURN.print(NORETURN,state)
        case STDCALL => printerSTDCALL.print(STDCALL,state)
        case x: Declspec => summon[Printer[Declspec]].print(x,state) 
        case x: KernelSpecifier => summon[Printer[KernelSpecifier]].print(x,state)

          
given printerINLINE: Printer[INLINE.type] = constWordPrinter("inline")
given printerNORETURN: Printer[NORETURN.type] = constWordPrinter("_Noreturn")
given printerSTDCALL: Printer[STDCALL.type] = constWordPrinter("__stdcall")

given Printer[Declspec] with
   def print(data: Declspec, state: PrintState): PrintState =
      state.addSmallBlock("__declspec").addSmallBlock("(").print(data.value).addSmallBlock(")")

given Printer[KernelSpecifier] = Printer.derived

given printerKERNEL: Printer[KERNEL.type] = constWordPrinter("kernel") 

given Printer[VecTypeHint] with
   def print(data: VecTypeHint, state: PrintState): PrintState =
     state.addSmallBlock("__attribute__")
          .addSmallBlock("((")
          .addSmallBlock("__vec_type_hint(")
          .print(data.typeName)
          .addSmallBlock(")")
          .addSmallBlock("))")   


given Printer[WorkGroupSizeHint] with
   def print(data: WorkGroupSizeHint, state: PrintState): PrintState =
     state.addSmallBlock("__attribute__")
          .addSmallBlock("((")
          .addSmallBlock("__work_group_size_hint(")
          .print(SeparatedList[IntConstant,PrintToken](data.sizes.map(IntConstant(_)),SmallBlock(",")))
          .addSmallBlock(")")
          .addSmallBlock("))")   


given Printer[StructOrUnionSpecifier] with
    
    def print(data: StructOrUnionSpecifier, state: PrintState): PrintState =
      val s1 = state.print(data.kind).addWhiteSpace().print(data.name)
      if (data.components.isEmpty) then
        s1
      else 
        s1.print(InBlock( 
                   TrailingSeparatedList(data.components, Tokens(SmallBlock(";"),NewLine) ),
                   "{", "}"
        ))
        
given Printer[StructOrUnion] = Printer.derived

given printerSTRUCT: Printer[STRUCT.type] = constWordPrinter("struct")
given printerUNION: Printer[UNION.type] = constWordPrinter("union")


given Printer[StructComponentDeclaration] with

    def print(data: StructComponentDeclaration, state: PrintState ) =
      state.print(TrailingSeparatedList[SpecifierQualifier,PrintToken](data.qualifiers, Whitespace))
           .print(SeparatedList[StructDeclarator,PrintToken](data.declarators, SmallBlock(", ")))
           .print(Tokens(SmallBlock(";"),NewLine))


given Printer[StructDeclarator] = Printer.derived

given Printer[BitFieldDeclarator] with

    def print(data: BitFieldDeclarator, state: PrintState) =
      state.print(data.base).addSmallBlock(":").addWhiteSpace().print(data.bits)

given Printer[EnumSpecifier] with

    def print(data: EnumSpecifier, state: PrintState): PrintState =
      val s1 = state.addSmallBlock("enum").addWhiteSpace()
               .print(data.name)
      if (data.enumerators.isEmpty) {
        s1
      } else {
        s1.print(InBlock(
           SeparatedList(data.enumerators, Tokens(SmallBlock(","), NewLine)),
            "{","}" ))
      }
    


given Printer[Enumerator] with

    def print(data: Enumerator, state: PrintState) =
      val s1 = state.print(data.name)
      data.value match 
        case Some(expr) => s1.addSmallBlock("=").addWhiteSpace().print(expr)
        case None => s1


given Printer[TypeDefName] = Printer.derived

given Printer[PointerTypeSpecifier] with

    def print(data: PointerTypeSpecifier, state: PrintState): PrintState =
      state.print(data.base).addWhiteSpace().print(data.pointer) 

given Printer[Pointer] with

    def print(data: Pointer, state: PrintState): PrintState =
      state.addSmallBlock("*")
           .print(SeparatedList[TypeQualifier,PrintToken](data.typeQual,Whitespace))
           .print(data.pointer)
      

given Printer[TypeName] with

    def print(data: TypeName, state: PrintState): PrintState =
      state.print(SeparatedList[SpecifierQualifier,PrintToken](data.specifiers,Whitespace))
           .print(data.declarator)
  


given Printer[Declarator] with

    def print(data: Declarator, state: PrintState) =
      state.print(data.pointer)
           .addWhiteSpace() 
           .print(data.base)

given Printer[DirectDeclarator] = Printer.derived

given Printer[WrappedDirectDeclarator] with
    def print(data: WrappedDirectDeclarator, state: PrintState) =
         state.addSmallBlock("(").print(data.declarator).addSmallBlock(")")

given Printer[ArrayDirectDeclarator] with
  def print(data: ArrayDirectDeclarator, state: PrintState) =
    state.print(data.base)
        .addSmallBlock("[")
        .print(SeparatedList[TypeQualifier,PrintToken](data.qualifers, Whitespace))
        .print(data.expression)
        .addSmallBlock("]")

given Printer[ArrayPointerDirectDeclarator] with
   def print(data: ArrayPointerDirectDeclarator, state: PrintState) =
     state.print(data.base)
         .addSmallBlock("[")
         .print(SeparatedList[TypeQualifier,PrintToken](data.qualifers, Whitespace))
         .addSmallBlock("*")
         .addSmallBlock("]")
 
given Printer[FunctionDirectDeclarator] with
   def print(data: FunctionDirectDeclarator, state: PrintState) =
    state.print(data.base)
         .addSmallBlock("(")
         .print(data.params)
         .addSmallBlock(")")
  
given Printer[BitFieldDirectDeclarator] with
    def print(data: BitFieldDirectDeclarator, state: PrintState) =
      state.print(data.name)
            .addSmallBlock(":").addWhiteSpace()
            .print(data.expr)
            
               
given Printer[FunctionPointerDirectDeclarator] with
    def print(data: FunctionPointerDirectDeclarator, state: PrintState) =
      state.addSmallBlock("(")
           .print(data.typeSpecifier)
           .addWhiteSpace()
           .print(data.pointer)
           .addWhiteSpace()
           .print(data.base)
           .addSmallBlock(")")
    
given Printer[ParameterTypeList] with
    def print(data: ParameterTypeList, state: PrintState) =
      val s1 = state.print(
        SeparatedList[ParameterDeclaration,Tokens[PrintToken]](data.parameters,Tokens(SmallBlock(","),Whitespace))
        )
      if (data.withVarargs) 
        s1.addSmallBlock("...")
      else
        s1  

given Printer[ParameterDeclaration] with
    def print(data: ParameterDeclaration, state: PrintState) =
      state.print(SeparatedList[DeclarationSpecifier,PrintToken](data.specifiers,Whitespace))
           .print(data.declarator)
          

given Printer[ParameterDeclarator] with
    def print(data: ParameterDeclarator, state: PrintState) =
      data match
        case x:AbstractDeclarator => summon[Printer[AbstractDeclarator]].print(x,state)
        case x:Declarator => summon[Printer[Declarator]].print(x,state)



given Printer[InitDeclarator] with
    
    def print(data: InitDeclarator, state:PrintState) =
      val s = state.print(data.decl)
      data.initializer match
        case Some(initializer) => s.addSmallBlock("=").print(initializer)
        case None => s

    

given Printer[CompoundStatement] with

   def print(data:CompoundStatement, state: PrintState) =
      val s1 = state.addSmallBlock("{").startBlock()
      val s2 = data.items.foldLeft(s1) { (s,e) =>
          s.print(e).addSmallBlock(";").addNewLine()
      }
      s2.finishBlock().addSmallBlock("}").addNewLine()

given Printer[BlockItem] with
    def print(data:BlockItem, state: PrintState) =
      data match
        case x: Statement => summon[Printer[Statement]].print(x,state)
        case x: Declaration => summon[Printer[Declaration]].print(x,state)


given Printer[Expression] with
    def print(data:Expression, state: PrintState) =
      data match
        case x:PrecAssigmentExpression => summon[Printer[PrecAssigmentExpression]].print(x,state)
        case x:CommaExpression => summon[Printer[CommaExpression]].print(x,state)

given Printer[CommaExpression] with        
  def print(data:CommaExpression, state: PrintState) =
    state.print(data.frs).addSmallBlock(",").addWhiteSpace().print(data.snd)


given Printer[PrecAssigmentExpression] with
  def print(data:PrecAssigmentExpression, state: PrintState) =
    data match
      case x:PrecConditionalExpression => summon[Printer[PrecConditionalExpression]].print(x,state)
      case x:AssigmentExpression => summon[Printer[AssigmentExpression]].print(x,state)

given Printer[AssigmentExpression] with
      def print(data:AssigmentExpression, state: PrintState) =
        state.print(data.frs).addWhiteSpace()
             .addSmallBlock(data.op.sign).addWhiteSpace()
             .print(data.snd)  

given Printer[PrecConditionalExpression] with
  def print(data:PrecConditionalExpression, state: PrintState) =
    data match
      case x:PrecLogicalOrExpression => summon[Printer[PrecLogicalOrExpression]].print(x,state)
      case x:ConditionalExpression => summon[Printer[ConditionalExpression]].print(x,state)

given Printer[ConditionalExpression] with
  def print(data:ConditionalExpression, state: PrintState) =
    state.print(data.cond).addWhiteSpace().addSmallBlock("?").addWhiteSpace()
         .print(data.frs).addWhiteSpace().addSmallBlock(":").addWhiteSpace()
         .print(data.snd)    

given Printer[BinaryExpression] with
  def print(data:BinaryExpression, state: PrintState) =
      state.print(data.frs)
            .addWhiteSpace().addSmallBlock(data.op.sign).addWhiteSpace()
            .print(data.snd)    
       

given Printer[PrecLogicalOrExpression] with
  def print(data:PrecLogicalOrExpression, state: PrintState) =
    data match
      case x:PrecLogicalAndExpression => summon[Printer[PrecLogicalAndExpression]].print(x,state)
      case x:LogicalOrBinaryExpression => summon[Printer[BinaryExpression]].print(x,state)
          
given Printer[PrecLogicalAndExpression] with
  def print(data:PrecLogicalAndExpression, state: PrintState) =
    data match
      case x:PrecInclusiveOrExpression => summon[Printer[PrecInclusiveOrExpression]].print(x,state)
      case x:LogicalAndBinaryExpression => summon[Printer[BinaryExpression]].print(x,state)
    
given Printer[PrecInclusiveOrExpression] with
  def print(data:PrecInclusiveOrExpression, state: PrintState) =
    data match
      case x:PrecExclusiveOrExpression => summon[Printer[PrecExclusiveOrExpression]].print(x,state)
      case x:InclusiveOrBinaryExpression => summon[Printer[BinaryExpression]].print(x,state)
    
given Printer[PrecExclusiveOrExpression] with
  def print(data:PrecExclusiveOrExpression, state: PrintState) =
    data match
      case x:PrecAndExpression => summon[Printer[PrecAndExpression]].print(x,state)
      case x:ExclusiveOrBinaryExpression => summon[Printer[BinaryExpression]].print(x,state)
    
given Printer[PrecAndExpression] with
  def print(data:PrecAndExpression, state: PrintState) =
    data match
      case x:EqualityExpression => summon[Printer[EqualityExpression]].print(x,state)
      case x:AndBinaryExpression => summon[Printer[BinaryExpression]].print(x,state)
    
given Printer[EqualityExpression] with
  def print(data:EqualityExpression, state: PrintState) =
    data match
      case x:RelationalExpression => summon[Printer[RelationalExpression]].print(x,state)
      case x:EqualityBinaryExpression => summon[Printer[BinaryExpression]].print(x,state)

given Printer[RelationalExpression] with
  def print(data:RelationalExpression, state: PrintState) =
    data match
      case x:ShiftExpression => summon[Printer[ShiftExpression]].print(x,state)
      case x:RelationalBinaryExpression => summon[Printer[BinaryExpression]].print(x,state)


given Printer[ShiftExpression] with
  def print(data:ShiftExpression, state: PrintState) =
    data match
      case x:AdditiveExpression => summon[Printer[AdditiveExpression]].print(x,state)
      case x:ShiftBinaryExpression => summon[Printer[BinaryExpression]].print(x,state)

given Printer[AdditiveExpression] with
  def print(data:AdditiveExpression, state: PrintState) =
    data match
      case x:MultiplicativeExpression => summon[Printer[MultiplicativeExpression]].print(x,state)
      case x:AdditiveBinaryExpression => summon[Printer[BinaryExpression]].print(x,state)


given Printer[MultiplicativeExpression] with
  def print(data:MultiplicativeExpression, state: PrintState) =
    data match
      case x:CastExpression => summon[Printer[CastExpression]].print(x,state)
      case x:MultiplicativeBinaryExpression => summon[Printer[BinaryExpression]].print(x,state)


given Printer[CastExpression] with
  def print(data:CastExpression, state: PrintState) =
    data match
      case x:UnaryExpression => summon[Printer[UnaryExpression]].print(x,state)
      case x:Cast => summon[Printer[Cast]].print(x,state)


given Printer[Cast] with
  def print(data:Cast, state: PrintState) =
      state.addSmallBlock("(").print(data.typeName).addSmallBlock(")").print(data.argument)


given Printer[UnaryExpression] with
  def print(data: UnaryExpression, state: PrintState) =
    data match
      case x:PostfixExpression => summon[Printer[PostfixExpression]].print(x,state)
      case x:PrefixIncrementExpression => summon[Printer[PrefixIncrementExpression]].print(x,state)
      case x:PrefixDecrementExpression => summon[Printer[PrefixDecrementExpression]].print(x,state)
      case x:UnaryOperatorExpression => summon[Printer[UnaryOperatorExpression]].print(x,state)
      case x:SizeofConstExpression => summon[Printer[SizeofConstExpression]].print(x,state)
      case x:SizeofTypeExpression => summon[Printer[SizeofTypeExpression]].print(x,state)


given Printer[PrefixIncrementExpression] with
  def print(data: PrefixIncrementExpression, state: PrintState) =
    state.addSmallBlock("++").print(data.base)

given Printer[PrefixDecrementExpression] with
  def print(data: PrefixDecrementExpression, state: PrintState) =
    state.addSmallBlock("--").print(data.base)

given Printer[UnaryOperatorExpression] with
  def print(data: UnaryOperatorExpression, state: PrintState) =
    state.addSmallBlock(data.op.sign).addWhiteSpace().print(data.argument)


given Printer[SizeofConstExpression] with
  def print(data: SizeofConstExpression, state: PrintState) =
    state.addSmallBlock("sizeof").addWhiteSpace().print(data.expression)


given Printer[SizeofTypeExpression] with
  def print(data: SizeofTypeExpression, state: PrintState) =
    state.addSmallBlock("sizeof").addSmallBlock("(").print(data.typeName).addSmallBlock(")")


//sealed trait PostfixExpression extends UnaryExpression

given Printer[PostfixExpression] with
  def print(data: PostfixExpression, state: PrintState) =
    data match
      case x:PrimaryExpression => summon[Printer[PrimaryExpression]].print(x,state)
      case x:ArrayIndexExpression => summon[Printer[ArrayIndexExpression]].print(x,state)
      case x:FunctionCallExpression => summon[Printer[FunctionCallExpression]].print(x,state)
      case x:DotSelectExpression => summon[Printer[DotSelectExpression]].print(x,state)
      case x:ArrowSelectExpression => summon[Printer[ArrowSelectExpression]].print(x,state)
      case x:PostfixIncrementExpression => summon[Printer[PostfixIncrementExpression]].print(x,state)
      case x:PostfixDecrementExpression => summon[Printer[PostfixDecrementExpression]].print(x,state)   
      case x:CompoundLiteral => summon[Printer[CompoundLiteral]].print(x,state)
   

given Printer[ArrayIndexExpression] with
  def print(data: ArrayIndexExpression, state: PrintState) =
    state.print(data.base).addSmallBlock("[").print(data.index).addSmallBlock("]") 


given Printer[FunctionCallExpression] with
  def print(data: FunctionCallExpression, state: PrintState) =
    state.print(data.fun)
         .addSmallBlock("(")
         .print(SeparatedList[AssigmentExpression,Tokens[PrintToken]](data.arguments,Tokens(SmallBlock(","),Whitespace)))
         .addSmallBlock(")") 


given Printer[DotSelectExpression] with
  def print(data: DotSelectExpression, state: PrintState) =
    state.print(data.qualifier)
         .addSmallBlock(".")
         .print(data.select)
         
given Printer[ArrowSelectExpression] with
  def print(data: ArrowSelectExpression, state: PrintState) =
    state.print(data.qualifier)
         .addSmallBlock("->")
         .print(data.select)

given Printer[PostfixIncrementExpression] with
  def print(data: PostfixIncrementExpression, state: PrintState) =
    state.print(data.base)
         .addSmallBlock("++")
         
given Printer[PostfixDecrementExpression] with
  def print(data: PostfixDecrementExpression, state: PrintState) =
    state.print(data.base)
         .addSmallBlock("--")


given Printer[CompoundLiteral] with
  def print(data: CompoundLiteral, state: PrintState) =
    state.addSmallBlock("(")
          .print(data.typeName)
          .addSmallBlock(")")
          .print(InBlock(
            SeparatedList[CompoundInitializerComponent,Tokens[PrintToken]](
                  data.initializers, Tokens(SmallBlock(","),Whitespace)),
            "{", "}"
          ))
          
          
given Printer[CompoundInitializerComponent] with
  def print(data: CompoundInitializerComponent, state: PrintState) =
    if (data.designators.isEmpty)
      state.print(data.initializer)
    else
      state.print(SeparatedList[Designator,PrintToken](data.designators, EmptyPrint))
           .addSmallBlock("=")
           .print(data.initializer)

given Printer[Designator] = Printer.derived 

given Printer[IndexDesignator] with
  def print(data: IndexDesignator, state: PrintState) =
    state.addSmallBlock("[").print(data.expression).addSmallBlock("]")


given Printer[DotDesignator] with
  def print(data: DotDesignator, state: PrintState) =
    state.addSmallBlock(".").print(data.identifier)


given Printer[PrimaryExpression] = Printer.derived

given Printer[Identifier] with
   def print(data: Identifier, state: PrintState): PrintState =
     state.addSmallBlock(data.value).addWhiteSpace()


given Printer[IntConstant] with
   def print(data: IntConstant, state: PrintState): PrintState =
     state.addSmallBlock(data.value.toString).addWhiteSpace()

given Printer[CharConstant] with
   def print(data: CharConstant, state: PrintState): PrintState =
     state.addSmallBlock(s"'${escapeChar(data.value)}'")
     
   def escapeChar(c: Char):String =
    if (c=='\'') {
      "\\\'"
    } else if (c=='\n') {
      "\\n"
    } else {
      c.toString
    }

    
given Printer[WrappedExpression] with
   def print(data: WrappedExpression, state: PrintState): PrintState =
     state.addSmallBlock("(").print(data.value).addSmallBlock(")")


given Printer[StringLiteral] with

    def print(data:StringLiteral, state: PrintState): PrintState =
      val r = "\"" + escape(data.value) + "\""
      state.addSmallBlock(r)

    def escape(s:String):String =
      val builder = new StringBuilder()
      val chars = s.toCharArray
      var i = 0
      while(i < chars.length) {
        val c = chars(i)
        if( c == '\'' )
            builder.append( "\\'" );
        else if ( c == '\"' )
            builder.append( "\\\"" );
        else if( c == '\r' )
            builder.append( "\\r" );
        else if( c == '\n' )
            builder.append( "\\n" );
        else if( c == '\t' )
            builder.append( "\\t" );
        else if( c == '"' )
            builder.append("\\'");
        else if( c == '\'' )
            builder.append("\\\"");
        else if( c < 32 || c >= 127 )
            // TODO: review unicode implem
            builder.append( String.format( "\\u%04x", c.toInt ) );
        else
            builder.append( c );
        i = i + 1
      }
      builder.toString()


given Printer[Statement] with 
  def print(data: Statement, state: PrintState): PrintState =
    data match
      case x:LabeledStatement => summon[Printer[LabeledStatement]].print(x,state)
      case x:CompoundStatement => summon[Printer[CompoundStatement]].print(x,state)
      case x:ExpressionStatement => summon[Printer[ExpressionStatement]].print(x,state)
      case x:SelectionStatement => summon[Printer[SelectionStatement]].print(x,state)
      case x:IterationStatement => summon[Printer[IterationStatement]].print(x,state)
      case x:JumpStatement => summon[Printer[JumpStatement]].print(x,state)
      case x:AsmStatement => summon[Printer[AsmStatement]].print(x,state)
      case STRAY_SEMICOLON => summon[Printer[STRAY_SEMICOLON.type]].print(STRAY_SEMICOLON, state)

given Printer[LabeledStatement] = Printer.derived

given Printer[GotoLabeledStatement] with
  def print(data: GotoLabeledStatement, state: PrintState) =
    state.print(data.label).addSmallBlock(":").addWhiteSpace().print(data.statement)

given Printer[CaseStatement] with
  def print(data: CaseStatement, state: PrintState) =
    state.addSmallBlock("case").addWhiteSpace()
         .print(data.expr).addSmallBlock(":").addWhiteSpace()
         .print(data.statement)
   
given Printer[DefaultStatement] with
  def print(data: DefaultStatement, state: PrintState) =
    state.addSmallBlock("default:").addWhiteSpace()
          .print(data.statement)
       

given Printer[ExpressionStatement] with
  def print(data: ExpressionStatement, state: PrintState) =
    state.print(data.expression).addSmallBlock(";")

given Printer[SelectionStatement] = Printer.derived

given Printer[IfStatement] with
  def print(data: IfStatement, state: PrintState) =
    val s1 = (
      state.addSmallBlock("if").addWhiteSpace()
            .addSmallBlock("(").print(data.condition).addSmallBlock(")").addWhiteSpace()
            .print(data.ifTrue)
    )
    data.ifElse match
      case Some(ifFalse) => 
        s1.addSmallBlock("else").addWhiteSpace()
          .print(ifFalse)
      case None => s1
 

given Printer[SwitchStatement] with
  def print(data: SwitchStatement, state: PrintState) =
    state.addSmallBlock("switch").addWhiteSpace()
         .addSmallBlock("(").print(data.expression).addSmallBlock(")").addWhiteSpace()
         .print(data.body)
  

given Printer[IterationStatement] = Printer.derived

given Printer[WhileStatement] with
  def print(data: WhileStatement, state: PrintState) =
    state.addSmallBlock("while").addWhiteSpace()
         .addSmallBlock("(").print(data.condition).addSmallBlock(")").addWhiteSpace()
         .print(data.body)


given Printer[DoStatement] with
  def print(data: DoStatement, state: PrintState) =
    state.addSmallBlock("do").addWhiteSpace()
         .print(data.body).addWhiteSpace()
         .addSmallBlock("while").addWhiteSpace()
         .addSmallBlock("(").print(data.condition).addSmallBlock(")")


given Printer[ForStatement] with
  def print(data: ForStatement, state: PrintState) =
    state.addSmallBlock("for")         
        .addSmallBlock("(")
        .print(data.init)
        .addSmallBlock(";")
        .print(data.condition)
        .addSmallBlock(";")
        .print(data.update)
        .addSmallBlock(")")
        .print(data.body)



given Printer[ForInit] with
  def print(data: ForInit, state: PrintState) =
    data match
      case x: Expression => summon[Printer[Expression]].print(x,state)
      case x: Declaration => summon[Printer[Declaration]].print(x,state)

given Printer[JumpStatement] = Printer.derived

given Printer[Goto] with
  def print(data: Goto, state: PrintState) =
    state.addSmallBlock("goto")
         .addWhiteSpace().print(data.label)         
         .addSmallBlock(";")
        

given continuePrinter: Printer[Continue.type] with
  def print(data: Continue.type, state: PrintState) =
    state.addSmallBlock("continue") 
         .addSmallBlock(";")         
       
given breakPrinter: Printer[Break.type] with
  def print(data: Break.type, state: PrintState) =
    state.addSmallBlock("break") 
          .addSmallBlock(";")         
       

given Printer[Return] with
  def print(data: Return, state: PrintState) =
    state.addSmallBlock("return")         
        .addWhiteSpace()
        .print(data.expression)
        .addSmallBlock(";")
        

given Printer[AsmStatement] with
  def print(data: AsmStatement, state: PrintState) =
    state.addSmallBlock("asm")         
        .addWhiteSpace()
        .addSmallBlock("(")
        .print(data.data)
        .addSmallBlock(")")
        .addSmallBlock(";")







      



      
      