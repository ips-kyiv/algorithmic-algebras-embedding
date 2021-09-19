package ips.clang


sealed trait C_Ast

sealed trait Expression extends C_Ast 

sealed trait PrimaryExpression extends PostfixExpression

case class Identifier(value:String)  extends PrimaryExpression with TypeDefName 

case class IntConstant(value: Int)  extends PrimaryExpression
case class CharConstant(value: Char)  extends PrimaryExpression
case class StringLiteral(value: String) extends PrimaryExpression

case class WrappedExpression(value: Expression)  extends PrimaryExpression

case class GenericSelection( qualifier: PrecAssigmentExpression, associations: List[Int] )


sealed trait PostfixExpression extends UnaryExpression

case class ArrayIndexExpression(base: PostfixExpression, index: Expression) extends PostfixExpression

case class FunctionCallExpression(fun: PostfixExpression, arguments: List[PrecAssigmentExpression]) extends PostfixExpression

case class DotSelectExpression(qualifier: PostfixExpression, select: Identifier) extends PostfixExpression

case class ArrowSelectExpression(qualifier: PostfixExpression, select: Identifier) extends PostfixExpression

case class PostfixIncrementExpression(base: PostfixExpression) extends PostfixExpression

case class PostfixDecrementExpression(base: PostfixExpression) extends PostfixExpression

case class CompoundLiteral(typeName: TypeName, initializers: List[Int]) extends PostfixExpression

sealed trait UnaryExpression extends CastExpression

case class PrefixIncrementExpression(base: UnaryExpression) extends UnaryExpression
case class PrefixDecrementExpression(base: UnaryExpression) extends UnaryExpression
case class UnaryOperatorExpression(op: UnaryOperator, argument: CastExpression) extends UnaryExpression
case class SizeofConstExpression(expression: UnaryExpression) extends UnaryExpression
case class SizeofTypeExpression(typeName: TypeName) extends UnaryExpression

enum UnaryOperator(val sign: String) {   
   case UNARY_PLUS extends UnaryOperator("+")
   case UNARY_AMPERSAND extends UnaryOperator("&")
   case UNARY_MULTIPLY extends UnaryOperator("*")
   case UNARY_MINUS extends UnaryOperator("-")
   case UNARY_INVERSE extends UnaryOperator("~")
   case UNARY_NOT extends UnaryOperator("!")
}


sealed trait CastExpression extends MultiplicativeExpression
case class Cast(typeName: TypeName, argument: CastExpression) extends CastExpression

sealed trait BinaryExpression {
  def op: String
  def frs: Expression
  def snd: Expression
}

sealed trait MultiplicativeExpression extends AdditiveExpression

case class MultiplicativeBinaryExpression(op: String, 
                                          frs: MultiplicativeExpression, 
                                          snd: CastExpression) extends MultiplicativeExpression
                                                                 with BinaryExpression



sealed trait AdditiveExpression extends ShiftExpression
case class AdditiveBinaryExpression(op: String, 
                                    frs: MultiplicativeExpression, 
                                    snd: CastExpression) extends MultiplicativeExpression with BinaryExpression



sealed trait ShiftExpression extends RelationalExpression
case class ShiftBinaryExpression(op: String,
      frs: MultiplicativeExpression, snd: CastExpression) extends MultiplicativeExpression with BinaryExpression

sealed trait RelationalExpression extends EqualityExpression
case class RelationalBinaryExpression(op: String, 
      frs: RelationalExpression, snd: ShiftExpression) extends RelationalExpression with BinaryExpression

sealed trait EqualityExpression extends PrecAndExpression
case class EqualityBinaryExpression(op: String, 
      frs: RelationalExpression, snd: ShiftExpression) extends EqualityExpression with BinaryExpression

sealed trait PrecAndExpression extends PrecExclusiveOrExpression
case class AndBinaryExpression(op: String,
     frs: PrecAndExpression, snd: EqualityExpression) extends PrecAndExpression with BinaryExpression

sealed trait PrecExclusiveOrExpression extends PrecInclusiveOrExpression
case class ExclusiveOrBinaryExpression(
    op: String, 
    frs: PrecExclusiveOrExpression, 
    snd: PrecAndExpression) extends PrecExclusiveOrExpression with BinaryExpression

sealed trait PrecInclusiveOrExpression extends PrecLogicalAndExpression
case class InclusiveOrBinaryExpression(op: String, 
          frs: PrecExclusiveOrExpression, 
          snd: PrecAndExpression) extends PrecInclusiveOrExpression with BinaryExpression
    
sealed trait PrecLogicalAndExpression extends PrecLogicalOrExpression
case class LogicalAndBinaryExpression(op: String, 
    frs: PrecLogicalAndExpression, 
    snd: PrecInclusiveOrExpression) extends PrecLogicalAndExpression with BinaryExpression
    
sealed trait PrecLogicalOrExpression extends PrecConditionalExpression
case class LogicalOrBinaryExpression(op: String, 
        frs: PrecLogicalAndExpression, 
        snd: PrecInclusiveOrExpression) extends PrecLogicalOrExpression with BinaryExpression
        
sealed trait PrecConditionalExpression extends PrecAssigmentExpression
case class ConditionalExpression(
        cond: PrecLogicalOrExpression, 
        frs: Expression, 
        snd: PrecConditionalExpression) extends PrecConditionalExpression 


sealed trait PrecAssigmentExpression extends Expression 
case class AssigmentExpression(
        op: String, 
        frs: UnaryExpression, 
        snd: PrecAssigmentExpression) extends PrecAssigmentExpression 


case class CommaExpression(frs: Expression, snd: Expression) extends Expression

case class Declarator(pointer: Option[Pointer], base: String)

case class TypeName(specifiers: List[SpecifierQualifier], declarator: Option[AbstractDeclarator])

sealed trait AbstractDeclarator 
case class Pointer( typeQual: List[SpecifierQualifier], pointer: Option[Pointer]) extends AbstractDeclarator

sealed trait SpecifierQualifier extends DeclarationSpecifier

case class EnumSpecifier(name: Option[Identifier], enumerators: List[Enumerator]) extends SpecifierQualifier

sealed trait TypeDefName


case class Enumerator(name: Identifier, value: Option[Expression])

case class StructComponentDeclaration(qualifiers: List[SpecifierQualifier], declarators: List[Int])

sealed trait DeclarationSpecifier


