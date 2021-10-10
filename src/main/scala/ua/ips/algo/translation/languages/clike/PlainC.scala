package ua.ips.algo.translation.languages.clike

import ua.ips.algo._


object PlainC extends CBase {

  val dialetcs = Set(Dialect.PlainC)
  
}

object CwithOpenCL {

  val dialetcs = Set(Dialect.PlainC, Dialect.OpenCL)

}