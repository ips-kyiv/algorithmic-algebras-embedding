package ua.ips.algo.translation.languages.clike


import ua.ips.algo._
import ua.ips.algo.translation._
import java.io._
import java.nio.file._


trait CBase extends Language {


  type AstDef = TranslationUnit


  def genContext(ctx: IRContext): OutputBundle = 
    {
      val name = generateName(ctx.fullName)
      val ast = genMainNode(ctx, name)

      println(s"ast: $ast" )
      OutputBundle("output",
        Map(
           s"${name}.c" -> ast,
           "CMakeLists.txt" ->  TranslationUnit(List(CMakeGen.lib(name)))
      ))
    }
  

  // yet not implemented
  val baseInterpretation: Interpretation = FreeInterpretation 

  def dataSortDef(dataSort: DataSort): AstDef = ???

  def constantDef(item: baseInterpretation.DataItem, sort: DataSort): AstDef = ???

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
    
  def generateName(fullName: Seq[String]): String =
    // for now - le't d
    fullName.mkString

  def genMainNode(irCtx: IRContext, name:String): TranslationUnit = {
     val ctx = CBaseGenContext(irCtx)
     val stdDecls = genStandardDeclarations(ctx)
     val mainDecl = genMainDeclaration(ctx, name) 
     val mainDecls: List[ExternalDeclaration] = List(mainDecl)
     val prototypes: List[ExternalDeclaration] = ctx.functionPrototypes.toList
     val defintions: List[ExternalDeclaration] = ctx.functionDefinitions.toList
     TranslationUnit(
       stdDecls ++ prototypes ++ mainDecls ++ defintions  
     )
  }

  def genStandardDeclarations(ctx: CBaseGenContext): List[ExternalDeclaration] = {
    List.empty
  }

  def genMainDeclaration(ctx: CBaseGenContext, name: String): ExternalDeclaration = {
     val parameters = ctx.irCtx.inputParams.flatMap(_.inputs).map(v =>
        ParameterDeclaration( genIVarSpecifiers(ctx, v), Declarator(None, IdentifierDeclarator(Identifier(v.name))) )
     ).toList
     val returnTypeSpecifiers = EXTERN::genTypeSpecifiers(ctx,ctx.irCtx.outputs(0).expr.sort) 
     FunctionDefinition(specifiers=returnTypeSpecifiers, //: List[DeclarationSpecifier], 
            declarator = Declarator(None, 
              FunctionDirectDeclarator(
                Identifier(name),
                ParameterTypeList(parameters, false) 
              )
            ), 
            declarations = List.empty, //: List[Declaration],  TODO: vars
            body = genCompoundStatement(ctx,  ctx.irCtx.rootNode)
      ) 
  }

  def genCompoundStatement(ctx: CBaseGenContext, node: IRNode): CompoundStatement = {
    node match
      case SeqIRNode(id,schema,childs) =>
          CompoundStatement(childs.flatMap(x => genBlockItems(ctx, x)).toList)
      case ParIRNode(id,schema,childs) =>  
          // extract each of parallel to function.
          // variabes are passed by pointers.
          val count = childs.length
          val (barrierDecl, barrierName) = genBarrierInit(id, ctx, count)
          val submitFunctionCalls: List[BlockItem] = childs.zipWithIndex.flatMap{
            (c, i) => genParFunctionSubmit(ctx, c,  barrierName)
          }.toList
          val barrierAwaitCall = genBarrierAwaitCall(barrierName, ctx)
          val barrierDecls: List[BlockItem] = List(barrierDecl)
          CompoundStatement( barrierDecls ++ submitFunctionCalls ++ List(barrierAwaitCall))
      case e: EmptyIRNode => CompoundStatement(List())
      case _ => 
          println(s"Generation of compount statement is not implemented for node $node")
          ???

  }

  def genIVarSpecifiers(ctx: CBaseGenContext, v: IRVar): List[SpecifierQualifier] =
      genTypeSpecifiers(ctx, v.init.origin.sort)
      


  /**
   * create in context definition and prototype and generate function call submitted.
   **/
  def genParFunctionSubmit(ctx: CBaseGenContext, node: IRNode, barrierName: String): List[Statement] = {
    println(s"genParFunctioNSubmit is not implemented for ${node}")
    ???
  }

  def genBarrierInit(id: String, ctx: CBaseGenContext, count: Int): (Declaration, String) =
    ???

  def genBarrierAwaitCall(barrierName: String, ctx: CBaseGenContext): Statement =
    ???

  def genBlockItems(ctx: CBaseGenContext, node: IRNode): List[BlockItem] =
    node match
      case seqNode: SeqIRNode => List(genCompoundStatement(ctx, seqNode))
      case parSeqNode: ParIRNode => List(genCompoundStatement(ctx, parSeqNode))
      case inputs@IRInputs(id, schema, childs) =>
        // inputs now are printed in the function def, so = skip.
        // childs.flatMap(x => genBlockItems(ctx,x)).toList
        List.empty
      case v@IRVar(id, schema, name, init) =>
        List(genVariableDeclaration(ctx, name, init.origin))
      case IROutput(id,schema, expr) =>
        //val outputCall = FunctionCallExpression(Identifier("output"), List(genPrecAssigmentExpression(ctx, expr)) )
        val cExpr = genPrecAssigmentExpression(ctx, expr)
        //List(ExpressionStatement(outputCall))
        List(Return(Some(cExpr)))
      case _ =>
        println(s"genBlockItem is not implemented for ${node}")
        ???  
  
  def genVariableDeclaration(ctx: CBaseGenContext, name: String, init: DataExpression): Declaration = 
      init.sort match 
        case basicSort: BasicDataSort =>
          genBasicVariableDeclaration(ctx, name, basicSort, init)
        case arrSort: FixedArrayDataSort =>
          genArrayVariableDeclaration(ctx, name, arrSort, init)
        case _ => 
          println(s"variable declaraor for sort ${init.sort} is not implemented yet");
          ???

  def genBasicVariableDeclaration(ctx: CBaseGenContext, name: String, 
                                   sort: BasicDataSort, init: DataExpression): Declaration =
      val specifiers: List[DeclarationSpecifier] = genTypeSpecifiers(ctx,init.sort)
      val baseDeclarator = IdentifierDeclarator(Identifier(name))
      val decl: Declarator = Declarator(None, baseDeclarator)
      val initializer = genPrecAssigmentExpression(ctx, init)
      val initDeclarator: InitDeclarator = InitDeclarator(decl, Some(initializer))
      Declaration(specifiers, List(initDeclarator))
      

  def genArrayVariableDeclaration(ctx: CBaseGenContext, name: String,
                                  sort: FixedArrayDataSort, init: DataExpression): Declaration = ???


  def genTypeSpecifiers(ctx: CBaseGenContext, sort: DataSort): List[SpecifierQualifier] =
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
          val baseDeclarations = genTypeSpecifiers(ctx, baseSort)
          ???
          // can't without direct clarator.
      case _ =>
        println(s"sort is not supported yet for C: $sort")
        ???    

  def genAssigment(ctx: CBaseGenContext, name: String, expr: DataExpression): AssigmentExpression = 
    AssigmentExpression(
      AssignmentOperator.ASSIGN,
      Identifier(name),
      genPrecAssigmentExpression(ctx, expr)
    )

  def genExpression(ctx: CBaseGenContext, expr: DataExpression): Expression = 
      expr match
        case DataAccessExpression(name, sort) =>
          // TODO: find var ?
          Identifier(name)
        case FunctionalExpression(signature, args) =>
          if (isSpecialSignature(signature)) then
            SpecialFunctionalExpression(signature, args.map(a => genPrecAssigmentExpression(ctx,a)).toList  )
          else
            FunctionCallExpression(Identifier(signature.name), args.map(a => genPrecAssigmentExpression(ctx, a)).toList )
        case ConstantExpression(name, value) =>
            generateConstantExpression(ctx, name, value)
        case DataInputExpression(name, sort, index) =>
          sort match
            case BasicDataSort(sortName) =>
               FunctionCallExpression(Identifier("read_"+sortName), List())
            case _ =>
              ???

  def genPrecAssigmentExpression(ctx: CBaseGenContext, dataExpr: DataExpression): PrecAssigmentExpression =
    val expr = genExpression(ctx, dataExpr)
    //expr match
    //  case e: PrecAssigmentExpression => e
    //  case other => WrappedExpression(other)
    if  expr.isInstanceOf[PrecAssigmentExpression]  then
        expr.asInstanceOf[PrecAssigmentExpression]
    else
        WrappedExpression(expr)


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
    //
    //expr match
    //  case mExpr: MultiplicativeExpression => mExpr
    //  case _ => WrappedExpression(expr)
    if (expr.isInstanceOf[MultiplicativeExpression]) then
        expr.asInstanceOf[MultiplicativeExpression]
    else
        WrappedExpression(expr)

  def toCastExpression(expr: Expression): CastExpression =
    //
    // dotty regression: see https://github.com/lampepfl/dotty/issues/13565
    //expr match
    //  case cExpr: CastExpression => cExpr
    //  case _ => WrappedExpression(expr)
    //
    if (expr.isInstanceOf[CastExpression]) then
       expr.asInstanceOf[CastExpression]
    else
       WrappedExpression(expr)


  def generateConstantExpression(vtx: CBaseGenContext, sort: DataSort, value: String): Expression =
    sort match
      case BasicDataSort(sortName) =>
        sortName match
          case IntBasicRep.name => IntConstant(value.toInt)
          //case StringBasicRep.name => StringConstant(value)
          case _ => ???
      case _ => ???



}
