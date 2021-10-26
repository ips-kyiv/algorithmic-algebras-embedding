package ua.ips.algo.translation

//TODO: add pos
class TranslationException(message: String, code: Int = TranslationException.GENERIC) extends RuntimeException

object TranslationException {

  final val GENERIC = 1


  final val OPTIMIZATION_FAILED = 100

}