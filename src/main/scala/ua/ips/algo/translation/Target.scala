package ua.ips.algo.translation

import ua.ips.algo._


case class Target(
  language: Language,
  limits: TargetLimits,
  costs: TargetCosts,
)


case class TargetLimits(
    hostMemory: Long,
    deviceMemory: Long,
    nHostCores: Int,
    nDevices: Int,
    nDeviceCores: Int,
)


case class TargetCosts(
    hostDeviceTransfer: Double,
    hostCopy: Double,
    hostCpuOperation: Double,
    deviceCpuOperation: Double,
)  
