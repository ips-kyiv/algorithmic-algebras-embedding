package ua.ips.algo.translation
import ua.ips.algo.*

//TODO: add pos
class TranslationException(message: String, code: Int = TranslationException.GENERIC, ex: Throwable = null) extends SchemaException(message, ex)

object TranslationException {

  final val GENERIC = 1


  final val OPTIMIZATION_FAILED = 100

}