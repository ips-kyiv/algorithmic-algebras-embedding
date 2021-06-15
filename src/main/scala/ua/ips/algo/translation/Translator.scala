package ua.ips.algo.translation

import ua.ips.algo._

class Translator(val target: Target)
{

   // TODO: ordering phases.
   // TODO: may-be tree.
   val optimizations: Seq[IROptimizationPhase] = Seq.empty

   def compile(input: Schema): target.language.OutputBundle =
      val optimized = optimize(input)
      codeGen(optimized)
      
   
   def optimize(input: Schema): IRContext =
      var ctx = new IRContext(target)
      val irNode = IRNode.accept(ctx,input,"root")
      // TODO: 
      for(phase <- optimizations) {
         val nextCtx = phase.apply(ctx);
         if !phase.mandatory && phase.applyIfBetterCostEstimation then
               val nextEstimations = nextCtx.rootNode.costEstimations(ctx)
               if (CostEstimations.betterThen(target, nextEstimations, ctx.rootNode.costEstimations(ctx))) then
                  ctx = nextCtx
         else
            ctx = nextCtx
      } 
      ctx

      
   def codeGen(ctx: IRContext): target.language.OutputBundle =
      target.language.genContext(ctx)
    
}





