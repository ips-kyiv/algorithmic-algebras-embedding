package ua.ips.algo

/**
* schema module:
* - situated in some package (ca be empty)
* - have some name
* - and define a schema itself.
**/
case class SchemaModule(
    packageName: Seq[String],
    name: String,
    schema: Schema
)