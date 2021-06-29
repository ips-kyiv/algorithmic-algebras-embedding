package ua.ips.tools.schema2c

import ua.ips.algo.*
import ua.ips.algo.translation.*

enum TargetExtensions:
  case PlainC, MMX, CUDA

case class TargetConfig(
    extentisons: List[TargetExtensions] = List(TargetExtensions.PlainC), 
    limits: TargetLimits  = defaultTargetLimits,
    costs: TargetCosts = TargetCosts() 
)

case class Schema2CConfig(
   inputJar: Option[String] = None,
   inputObject: String = "schemas.examples.Default",
   outputDir: String = "c-output",
   target: TargetConfig = TargetConfig()
)