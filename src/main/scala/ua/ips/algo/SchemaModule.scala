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
)  {

  def extractSignature(): DataSortSignature = schema.extractSignature(packageName, name)

}