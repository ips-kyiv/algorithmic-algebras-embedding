package ips.clang


sealed trait C_Ast

sealed trait Expression extends C_Ast with ForInit

sealed trait PrimaryExpression extends PostfixExpression

case class Identifier(value:String)  extends PrimaryExpression with TypeDefName with DirectDeclarator

case class IntConstant(value: Int)  extends PrimaryExpression
case class CharConstant(value: Char)  extends PrimaryExpression
case class StringLiteral(value: String) extends PrimaryExpression

case class WrappedExpression(value: Expression)  extends PrimaryExpression

case class GenericSelection( qualifier: PrecAssigmentExpression, associations: List[GenericAssociation] )

sealed trait GenericAssociation
case class TypeNameGenericAssociation(typeName: TypeName, assigment: PrecAssigmentExpression) extends GenericAssociation
case class DefaultGenericAssociation(assigment: PrecAssigmentExpression) extends GenericAssociation

sealed trait PostfixExpression extends UnaryExpression

case class ArrayIndexExpression(base: PostfixExpression, index: Expression) extends PostfixExpression

case class FunctionCallExpression(fun: PostfixExpression, arguments: List[PrecAssigmentExpression]) extends PostfixExpression

case class DotSelectExpression(qualifier: PostfixExpression, select: Identifier) extends PostfixExpression

case class ArrowSelectExpression(qualifier: PostfixExpression, select: Identifier) extends PostfixExpression

case class PostfixIncrementExpression(base: PostfixExpression) extends PostfixExpression

case class PostfixDecrementExpression(base: PostfixExpression) extends PostfixExpression

case class CompoundLiteral(typeName: TypeName, initializers: List[CompoundInitializerComponent]) extends PostfixExpression

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
  def op: BinaryOperator
  def frs: Expression
  def snd: Expression
}

sealed trait MultiplicativeExpression extends AdditiveExpression
case class MultiplicativeBinaryExpression(op: BinaryOperator, 
                                          frs: MultiplicativeExpression, 
                                          snd: CastExpression) extends MultiplicativeExpression
                                                                 with BinaryExpression

sealed trait AdditiveExpression extends ShiftExpression
case class AdditiveBinaryExpression(op: BinaryOperator, 
                                    frs: MultiplicativeExpression, 
                                    snd: CastExpression) extends MultiplicativeExpression with BinaryExpression

sealed trait ShiftExpression extends RelationalExpression
case class ShiftBinaryExpression(op: BinaryOperator, 
      frs: MultiplicativeExpression, snd: CastExpression) extends MultiplicativeExpression with BinaryExpression

sealed trait RelationalExpression extends EqualityExpression
case class RelationalBinaryExpression(op: BinaryOperator, 
      frs: RelationalExpression, snd: ShiftExpression) extends RelationalExpression with BinaryExpression

sealed trait EqualityExpression extends PrecAndExpression
case class EqualityBinaryExpression(op: BinaryOperator, 
      frs: RelationalExpression, snd: ShiftExpression) extends EqualityExpression with BinaryExpression

sealed trait PrecAndExpression extends PrecExclusiveOrExpression
case class AndBinaryExpression(op: BinaryOperator,
     frs: PrecAndExpression, snd: EqualityExpression) extends PrecAndExpression with BinaryExpression

sealed trait PrecExclusiveOrExpression extends PrecInclusiveOrExpression
case class ExclusiveOrBinaryExpression(
    op: BinaryOperator, 
    frs: PrecExclusiveOrExpression, 
    snd: PrecAndExpression) extends PrecExclusiveOrExpression with BinaryExpression

sealed trait PrecInclusiveOrExpression extends PrecLogicalAndExpression
case class InclusiveOrBinaryExpression(op: BinaryOperator, 
          frs: PrecExclusiveOrExpression, 
          snd: PrecAndExpression) extends PrecInclusiveOrExpression with BinaryExpression
    
sealed trait PrecLogicalAndExpression extends PrecLogicalOrExpression
case class LogicalAndBinaryExpression(op: BinaryOperator, 
    frs: PrecLogicalAndExpression, 
    snd: PrecInclusiveOrExpression) extends PrecLogicalAndExpression with BinaryExpression
    
sealed trait PrecLogicalOrExpression extends PrecConditionalExpression
case class LogicalOrBinaryExpression(op: BinaryOperator, 
        frs: PrecLogicalAndExpression, 
        snd: PrecInclusiveOrExpression) extends PrecLogicalOrExpression with BinaryExpression
        
sealed trait PrecConditionalExpression extends PrecAssigmentExpression
case class ConditionalExpression(
        cond: PrecLogicalOrExpression, 
        frs: Expression, 
        snd: PrecConditionalExpression) extends PrecConditionalExpression 

enum BinaryOperator(val sign: String, val priority: Int) {
  case BINARY_MULTIIPLY extends BinaryOperator("*", 1)
  case BINARY_DIVIDE extends BinaryOperator("/",1)
  case BINARY_MODULE extends BinaryOperator("%",1)
  case BINARY_PLUS extends BinaryOperator("+", 2)
  case BINARY_MINUS extends BinaryOperator("-", 2)
  case LEFT_SHIFT extends BinaryOperator("<<",3)
  case RIGHT_SHIFT extends BinaryOperator(">>",3)
  case LESS extends BinaryOperator("<", 4)
  case GREATER extends BinaryOperator(">",4)
  case LESS_EQ extends BinaryOperator("<=",4)
  case GREATER_EQ extends BinaryOperator(">=", 4)
  case EQ extends BinaryOperator("==",5)
  case NEQ extends BinaryOperator("!=",5)
  case AND extends BinaryOperator("&",6)
  case XOR extends BinaryOperator("^",7)
  case OR extends BinaryOperator("|",8)
  case LOGICAL_AND extends BinaryOperator("&&",9)
}

sealed trait PrecAssigmentExpression extends Expression 
case class AssigmentExpression(
        op: AssignmentOperator, 
        frs: UnaryExpression, 
        snd: PrecAssigmentExpression) extends PrecAssigmentExpression 


enum AssignmentOperator(val sign: String){
  case ASSIGN extends AssignmentOperator("*")
  case MULTIPLY_ASSIGN extends AssignmentOperator("*=")
  case DIVIDE_ASSIGN extends AssignmentOperator("/=")
  case MODULE_ASSIGN extends AssignmentOperator("%=")
  case PLUS_ASSIGN extends AssignmentOperator("+=")
  case MINUS_ASSIGN extends AssignmentOperator("-=")
  case LEFT_SHIFT_ASSIGN extends AssignmentOperator("<<=")
  case RIGHT_SHIFT_ASSIGN extends AssignmentOperator(">>=")
  case AND_ASSIGN extends AssignmentOperator("&=")
  case OR_ASSIGN extends AssignmentOperator("|=")
  case XOR_ASSIGN extends AssignmentOperator("ˆ=")
}

case class CommaExpression(frs: Expression, snd: Expression) extends Expression


case class Declarator(pointer: Option[Pointer],
                      base: DirectDeclarator) extends StructDeclarator 

case class TypeName(specifiers: List[SpecifierQualifier], declarator: Option[AbstractDeclarator])

sealed trait AbstractDeclarator 
case class Pointer( typeQual: List[SpecifierQualifier], pointer: Option[Pointer]) extends AbstractDeclarator
sealed trait DirectDeclarator
case class IdentifierDeclarator(id: Identifier) extends DirectDeclarator
case class WrappedDirectDeclarator(declarator: Declarator) extends DirectDeclarator

case class FunctionDirectDeclarator(base: DirectDeclarator, params: ParameterTypeList ) extends DirectDeclarator
case class BitFieldDirectDeclarator(name: Identifier, expr: Expression) extends DirectDeclarator
case class FunctionPointerDirectDeclarator(typeSpecifier: Option[TypeSpecifier], pointer: Pointer, base: DirectDeclarator) extends DirectDeclarator


sealed trait SpecifierQualifier extends DeclarationSpecifier
sealed trait TypeSpecifier extends SpecifierQualifier 

case class EnumSpecifier(name: Option[Identifier], enumerators: List[Enumerator]) extends TypeSpecifier

sealed trait TypeDefName

case class PointerTypeSpecifier(base: TypeSpecifier, pointer: Pointer) extends TypeSpecifier

case class Enumerator(name: Identifier, value: Option[Expression])

case class StructComponentDeclaration(qualifiers: List[SpecifierQualifier], declarators: List[StructDeclarator])

sealed trait DeclarationSpecifier


sealed trait StructDeclarator
case class BitFieldDeclarator(base: Declarator, bits: Expression) extends StructDeclarator

case class ParameterTypeList(parameters: List[ParameterDeclaration], withVarargs: Boolean)

case class ParameterDeclaration(specifiers: List[DeclarationSpecifier], declarator: Declarator)


sealed trait BlockItem 

sealed trait Statement extends BlockItem

sealed trait LabeledStatement extends Statement
case class GotoLabeledStatement(label: Identifier, statement: Statement) extends LabeledStatement
case class CaseStatement(expr: Expression, statement: Statement) extends LabeledStatement
case class DefaultStatement(statement: Statement) extends LabeledStatement

case class CompoundStatement(items: List[BlockItem]) extends Statement

case class ExpressionStatement(expression: Expression) extends Statement

sealed trait SelectionStatement extends Statement
case class IfStatement(condition: Expression, ifTrue: Statement, ifElse: Option[Statement]) extends SelectionStatement
case class SwitchStatement(expression: Expression, body: Statement)

sealed trait IterationStatement extends Statement
case class WhileStatement(condition: Expression, body: Statement) extends IterationStatement
case class DoStatement(body: Statement, condition: Expression) extends IterationStatement
case class ForStatement(init: Option[ForInit], condition: Option[Expression], update:Option[Expression], body: Statement)  extends IterationStatement

sealed trait ForInit


case class Declaration(specifiers: List[DeclarationSpecifier], 
                       initDeclarators: List[InitDeclarator]) extends ExternalDeclaration with BlockItem
                                                                                         with ForInit

case class InitDeclarator(decl: Declarator, initializer: Option[Initializer])

type Initializer = PrecAssigmentExpression
case class CompoundInitializer(components: List[CompoundInitializerComponent])

case class CompoundInitializerComponent(designators: List[Int], initializer: Initializer)


case class StaticAssert(expression: Expression, message: StringLiteral) extends ExternalDeclaration

case class FunctionDefinition(specifiers: List[DeclarationSpecifier], 
    declarator: Declarator, 
    declarations: List[Declaration],
    body: CompoundStatement) extends ExternalDeclaration


case object STRAY_SEMICOLON extends ExternalDeclaration with Statement

case class TranslationUnit(declarations: List[ExternalDeclaration])

sealed trait ExternalDeclaration

case class StringLob(value:String) extends ExternalDeclaration




