package ua.ips.algo

// TODO: add pos
class SchemaException(message:String, ex:Throwable = null) extends Exception(message,ex)

class SchemaBuildException(message:String, ex: Throwable = null) extends SchemaException(message, ex)

class SchemaSignatureBuildException(message:String) extends SchemaException(message)


