package ua.ips.algo.translation

import ua.ips.algo.*
import ua.ips.algo.util.*



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

val defaultTargetLimits = TargetLimits(
  hostMemory = 16 * MemoryUnits.Gb,
  deviceMemory = 8 * MemoryUnits.Gb,
  nHostCores = 4,
  nDevices = 1,
  nDeviceCores = 256
)


case class TargetCosts(
    hostDeviceTransfer: Double = 1,
    hostCopy: Double = 1,
    hostCpuOperation: Double = 1,
    deviceCpuOperation: Double = 1,
)  
