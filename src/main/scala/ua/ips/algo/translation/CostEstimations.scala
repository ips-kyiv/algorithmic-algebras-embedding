package ua.ips.algo.translation

import ua.ips.algo._



case class CostEstimations(
  hostDeviceTransfer: Long,
  hostCopy: Long,
  hostCpuOperation: Long,
  deviceCpuOperation: Long,
  contextSwitch: Long,
  maxHostThreads: Long,
  maxDeviceThreads: Long
) {

  def seqMerge(other: CostEstimations): CostEstimations =
    CostEstimations(
      hostDeviceTransfer = hostDeviceTransfer + other.hostDeviceTransfer,
      hostCopy = hostCopy + other.hostCopy,
      hostCpuOperation = hostCpuOperation + other.hostCpuOperation,
      deviceCpuOperation = deviceCpuOperation + other.deviceCpuOperation,
      contextSwitch = contextSwitch + other.contextSwitch,
      maxHostThreads = Math.max(maxHostThreads, other.maxHostThreads),
      maxDeviceThreads = Math.max(maxDeviceThreads, other.maxDeviceThreads)
    )

  def parMerge (other: CostEstimations): CostEstimations =
    CostEstimations(
      hostDeviceTransfer = Math.max(hostDeviceTransfer, other.hostDeviceTransfer),
      hostCopy = Math.max(hostCopy, other.hostCopy),
      hostCpuOperation = Math.max(hostCpuOperation, other.hostCpuOperation),
      deviceCpuOperation = Math.max(deviceCpuOperation, other.deviceCpuOperation),
      contextSwitch = Math.max(contextSwitch, other.contextSwitch) + 1,
      maxHostThreads = maxHostThreads + other.maxHostThreads,
      maxDeviceThreads = maxDeviceThreads + other.maxDeviceThreads 
    )

  def altMerge (other: CostEstimations): CostEstimations =
    CostEstimations(
      hostDeviceTransfer = Math.max(hostDeviceTransfer, other.hostDeviceTransfer),
      hostCopy = Math.max(hostCopy, other.hostCopy),
      hostCpuOperation = Math.max(hostCpuOperation, other.hostCpuOperation),
      deviceCpuOperation = Math.max(deviceCpuOperation, other.deviceCpuOperation),
      contextSwitch = Math.max(contextSwitch, other.contextSwitch),
      maxHostThreads = Math.max(maxHostThreads, other.maxHostThreads),
      maxDeviceThreads = Math.max(maxDeviceThreads, other.maxDeviceThreads)
    )
  

}

object CostEstimations:

  val ZERO = CostEstimations(
    hostDeviceTransfer = 0L,
    hostCopy = 0L,
    hostCpuOperation = 0L,
    deviceCpuOperation = 0L,
    contextSwitch = 0L,
    maxHostThreads = 0L,
    maxDeviceThreads = 0L
  )

  /**
  * true if x better then y on target 'target'
  **/
  def betterThen(target: Target, x: CostEstimations, y: CostEstimations) = false





