
package ua.ips.algo.translation.languages.clike

import scala.collection.mutable.ArrayBuffer

import ua.ips.algo._
import ua.ips.algo.translation._


class CBaseGenContext(val irCtx: IRContext) {

   val includes: ArrayBuffer[String] = ArrayBuffer()

   val functionPrototypes: ArrayBuffer[ExternalDeclaration] = ArrayBuffer()

   val functionDefinitions: ArrayBuffer[ExternalDeclaration] = ArrayBuffer()

}