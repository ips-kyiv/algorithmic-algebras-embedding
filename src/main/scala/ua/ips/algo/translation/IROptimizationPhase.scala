package ua.ips.algo.translation

import ua.ips.algo._
import ua.ips.algo.util._


trait IROptimizationPhase {

   def debugName: String;
   def mandatory: Boolean;
   def applyIfBetterCostEstimation: Boolean;

   def apply(ctx: IRContext): IRContext;


}