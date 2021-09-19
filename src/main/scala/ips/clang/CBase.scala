package ua.ips.algo.translation.languages.gpuc



trait CBase  {



  def toCastExpression(expr: Expression): CastExpression =
    expr match
      case cExpr: CastExpression => cExpr
      case _ => WrappedExpression(expr)



}
