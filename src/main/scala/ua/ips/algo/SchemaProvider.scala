package ua.ips.algo

/**
* Base traits for schema 
**/
trait SchemaProvider:

   def  defaultSchema: Schema

   def  resolve(name: String): Option[Schema] = None

