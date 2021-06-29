package ua.ips.algo.translation.languages.gpuc


import ua.ips.algo._
import ua.ips.algo.translation._
import java.io._
import java.nio.file._


trait CBase extends Language {

  type AstDef = TranslationUnit


  def genContext(ctx: IRContext): OutputBundle = 
    {
      val name = "Main.c"
      val ast = genMainNode(ctx)
      println(s"ast: $ast" )
      OutputBundle("output",Map(name -> ast))
    }
  
  // yet not implemented
  val baseInterpretation: Interpretation = FreeInterpretation 

  def dataSortDef(dataSort: DataSort): AstDef = ???

  def constantDef(item: baseInterpretation.DataItem): AstDef = ???

  def signatureDef(items: Seq[AstDef]): AstDef = ???
  
  //
  //def predicateDef(): Seq[AstDef]


  def run(bundle: OutputBundle): baseInterpretation.DataItem = ???


  def write(bundle: OutputBundle, dataDir: String): Unit =
    val path = Path.of(dataDir)
    for((name,ast) <- bundle.compilationUnits) {
      val file = outputFile(path, name)
      try {
        val startState = PrintState.create()
        val finishState = ast.print(startState)
        val s = finishState.collectBlock().value
        println(s"file $name:")
        println("BEGIN-----------------------------")
        println(s)
        println("END-----------------------------")
        file.print(s);
      } finally {
        file.close()
      }
    }

  def outputFile(dataDir: Path, fname: String): PrintWriter = 
    val nPath = dataDir.resolve(fname);
    val bf = Files.newBufferedWriter(nPath,StandardOpenOption.CREATE,StandardOpenOption.TRUNCATE_EXISTING)
    new PrintWriter(bf)
    

  def genMainNode(ctx: IRContext): TranslationUnit = {
     val printContext = CBasePrintContext()
     val stdDecls = genStandardDeclarations(ctx,printContext)
     val mainDecl = genMainDeclaration(ctx, printContext) 
     val mainDecls: List[ExternalDeclaration] = List(mainDecl)
     val prototypes: List[ExternalDeclaration] = printContext.functionPrototypes.toList
     val defintions: List[ExternalDeclaration] = printContext.functionDefinitions.toList
     TranslationUnit(
       stdDecls ++ prototypes ++ mainDecls ++ defintions  
     )
  }

  def genStandardDeclarations(irCtx: IRContext, pCtx: CBasePrintContext): List[ExternalDeclaration] = {
    List.empty
  }

  def genMainDeclaration(ctx: IRContext, pCtx: CBasePrintContext): ExternalDeclaration = {
     FunctionDefinition(specifiers=List.empty, //: List[DeclarationSpecifier], 
            declarator = Declarator(None, 
              FunctionDirectDeclarator(
                Identifier("main"),
                ParameterTypeList(List.empty, false)  // TODO:  generate params for char** args
              )
            ), 
            declarations = List.empty, //: List[Declaration],  TODO: vars
            body = genCompoundStatement(ctx, pCtx: CBasePrintContext,  ctx.rootNode)
      ) 
  }

  def genCompoundStatement(irCtx: IRContext, pCtx: CBasePrintContext, node: IRNode): CompoundStatement = {
    node match
      case SeqIRNode(id,schema,childs) =>
          CompoundStatement(childs.flatMap(x => genBlockItems(irCtx, pCtx, x)).toList)
      case ParIRNode(id,schema,childs) =>
          // extract each of parallel to function.
          // variabes are passed by pointers.
          val count = childs.length
          val (barrierDecl, barrierName) = genBarrierInit(id, pCtx, count)
          val submitFunctionCalls: List[BlockItem] = childs.zipWithIndex.flatMap{
            (c, i) => genParFunctionSubmit(irCtx,pCtx, c,  barrierName)
          }.toList
          val barrierAwaitCall = genBarrierAwaitCall(barrierName, pCtx)
          val barrierDecls: List[BlockItem] = List(barrierDecl)
          CompoundStatement( barrierDecls ++ submitFunctionCalls ++ List(barrierAwaitCall))
      case e: EmptyIRNode => CompoundStatement(List())
      case _ => 
          println(s"Generation of compount statement is not implemented for node $node")
          ???

  }

  /**
   * create in context definition and prototype and generate function call submitted.
   **/
  def genParFunctionSubmit(irCtx: IRContext, pCtx: CBasePrintContext, node: IRNode, barrierName: String): List[Statement] = {
    println(s"genParFunctioNSubmit is not implemented for ${node}")
    ???
  }

  def genBarrierInit(id: String, pCtx: CBasePrintContext, count: Int): (Declaration, String) =
    ???

  def genBarrierAwaitCall(barrierName: String, pCtx: CBasePrintContext): Statement =
    ???

  def genBlockItems(irCtx: IRContext, pCtx: CBasePrintContext, node: IRNode): List[BlockItem] =
    node match
      case seqNode: SeqIRNode => List(genCompoundStatement(irCtx, pCtx, seqNode))
      case parSeqNode: ParIRNode => List(genCompoundStatement(irCtx, pCtx, parSeqNode))
      case inputs@IRInputs(id, schema, childs) =>
        childs.flatMap(x => genBlockItems(irCtx,pCtx,x)).toList
      case v@IRVar(id, schema, name, init) =>
        List(genVariableDeclaration(irCtx, pCtx, name, init.origin))
      case IROutput(id,schema, expr) =>
        val outputCall = FunctionCallExpression(Identifier("output"), List(genPrecAssigmentExpression(irCtx, pCtx, expr)) )
        List(ExpressionStatement(outputCall))
      case _ =>
        println(s"genBlockItem is not implemented for ${node}")
        ???  
  
  def genVariableDeclaration(irCtx: IRContext, pCtx: CBasePrintContext, name: String, init: DataExpression): Declaration = 
      init.sort match 
        case basicSort: BasicDataSort =>
          genBasicVariableDeclaration(irCtx, pCtx, name, basicSort, init)
        case arrSort: FixedArrayDataSort[_] =>
          genArrayVariableDeclaration(irCtx, pCtx, name, arrSort, init)
        case _ => 
          println(s"variable declaraor for sort ${init.sort} is not implemented yet");
          ???

  def genBasicVariableDeclaration(irCtx: IRContext, pCtx: CBasePrintContext, name: String, 
                                   sort: BasicDataSort, init: DataExpression): Declaration =
      val specifiers: List[DeclarationSpecifier] = genTypeSpecifiers(irCtx,pCtx,init.sort)
      val baseDeclarator = IdentifierDeclarator(Identifier(name))
      val decl: Declarator = Declarator(None, baseDeclarator)
      val initializer = genPrecAssigmentExpression(irCtx, pCtx, init)
      val initDeclarator: InitDeclarator = InitDeclarator(decl, Some(initializer))
      Declaration(specifiers, List(initDeclarator))
      

  def genArrayVariableDeclaration(irCtx: IRContext, pCtx: CBasePrintContext, name: String,
                                  sort: FixedArrayDataSort[_], init: DataExpression): Declaration = ???


  def genTypeSpecifiers(irCtx: IRContext, pCtx: CBasePrintContext, sort: DataSort): List[DeclarationSpecifier] =
    sort match
      case BasicDataSort(name) =>
        name match
          case BooleanBasicRep.name => List(_BOOL)
          case IntBasicRep.name => List(INT)
          case FloatBasicRep.name => List(FLOAT)
          case DoubleBasicRep.name => List(DOUBLE)
          case _ =>
            throw new IllegalArgumentException(s"Unsupported basic sort: $name")
      case FixedArrayDataSort(length, baseSort) =>
          val baseDeclarations = genTypeSpecifiers(irCtx,pCtx, baseSort)
          ???
          // can't without direct clarator.
      case _ =>
        println(s"sort is not supported yet for C: $sort")
        ???    

  def genAssigment(irCtx: IRContext, pCtx: CBasePrintContext, name: String, expr: DataExpression): AssigmentExpression = 
    AssigmentExpression(
      AssignmentOperator.ASSIGN,
      Identifier(name),
      genPrecAssigmentExpression(irCtx, pCtx, expr)
    )

  def genExpression(irCtx: IRContext, pCtx: CBasePrintContext, expr: DataExpression): Expression = 
      expr match
        case DataAccessExpression(name, sort) =>
          // TODO: find var ?
          Identifier(name)
        case FunctionalExpression(signature, args) =>
          if (isSpecialSignature(signature)) then
            SpecialFunctionalExpression(signature, args.map(a => genPrecAssigmentExpression(irCtx,pCtx,a)).toList  )
          else
            FunctionCallExpression(Identifier(signature.name), args.map(a => genPrecAssigmentExpression(irCtx, pCtx, a)).toList )
        case ConstantExpression(name, value) =>
            generateConstantExpression(irCtx, pCtx, name, value)
        case DataInputExpression(name, sort, index) =>
          sort match
            case BasicDataSort(sortName) =>
               FunctionCallExpression(Identifier("read_"+sortName), List())
            case _ =>
              ???

  def genPrecAssigmentExpression(irCtx: IRContext, pCtx: CBasePrintContext, expr: DataExpression): PrecAssigmentExpression =
    genExpression(irCtx, pCtx, expr) match
      case e: PrecAssigmentExpression => e
      case other => WrappedExpression(other)


  def isSpecialSignature(signature: DataSortSignature): Boolean =
    signature.out match
      case BasicDataSort(name) =>
        signature.name == "+" || signature.name == "-" || signature.name == "*" || signature.name == "/" ||
        signature.name == "%" || signature.name == "&" || signature.name == "|" || signature.name == "=="
      case _ => false

  def SpecialFunctionalExpression(signature: DataSortSignature, args: List[PrecAssigmentExpression]):PrecAssigmentExpression =
    signature.name match
      case "*"  => MultiplicativeBinaryExpression(BinaryOperator.BINARY_MULTIIPLY, 
                                                                    toMultiplicativeExpression(args(0)), 
                                                                    toCastExpression(args(1)) )
      case "/"  => MultiplicativeBinaryExpression(BinaryOperator.BINARY_DIVIDE, 
                                                        toMultiplicativeExpression(args(0)), toCastExpression(args(1))) 
      case "+"  => AdditiveBinaryExpression(BinaryOperator.BINARY_PLUS, toMultiplicativeExpression(args.head), toCastExpression(args.tail.head))
      case "-"  => AdditiveBinaryExpression(BinaryOperator.BINARY_MINUS, toMultiplicativeExpression(args.head), toCastExpression(args.tail.head))
      case _ =>
        println(s"special expression ${signature.name}  is not implemented")
        ???  


  def toMultiplicativeExpression(expr: Expression): MultiplicativeExpression =
    expr match
      case mExpr: MultiplicativeExpression => mExpr
      case _ => WrappedExpression(expr)

  def toCastExpression(expr: Expression): CastExpression =
    expr match
      case cExpr: CastExpression => cExpr
      case _ => WrappedExpression(expr)


  def generateConstantExpression(irCtx: IRContext, pCtx: CBasePrintContext, sort: DataSort, value: String): Expression =
    sort match
      case BasicDataSort(sortName) =>
        sortName match
          case IntBasicRep.name => IntConstant(value.toInt)
          //case StringBasicRep.name => StringConstant(value)
          case _ => ???
      case _ => ???



}
