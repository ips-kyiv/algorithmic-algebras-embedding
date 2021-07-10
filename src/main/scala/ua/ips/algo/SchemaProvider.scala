package ua.ips.algo

/**
* Base traits for schema provider (i.e. wrapper wich represent file directories in schemas, ertc)
**/
trait SchemaProvider:

   def  defaultSchema: Schema

   /**
    * resilve schema with the given name.  
    * if schema found, than last name of fullName is a schema name,
    * all other - package name. 
    **/
   def  resolve(fulName: Seq[String]): Option[SchemaModule] = None

