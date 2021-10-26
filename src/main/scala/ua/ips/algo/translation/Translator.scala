package ua.ips.algo.translation

import ua.ips.algo._

class Translator(val target: Target)
{

   // TODO: ordering phases.
   // TODO: may-be tree.
   val optimizations: Seq[IROptimizationPhase] = Seq.empty

   def compile(input: SchemaModule): target.language.OutputBundle =
      println("Translator Input: "+input)
      val optimized = optimize(input)
      codeGen(optimized)
      
   
   def optimize(input: SchemaModule): IRContext =
      var ctx = new IRContext(target, input.packageName :+ input.name, Seq() )
      val irNode = IRNode.accept(ctx,input.schema,  IRContext.rootId)
      println("IRNode: "+ irNode)
      println("ctx root before optimize: " + ctx.rootNode)
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
      println("ctx root after optimize: " + ctx.rootNode) 
      ctx

      
   def codeGen(ctx: IRContext): target.language.OutputBundle =
      target.language.genContext(ctx)
    
}





