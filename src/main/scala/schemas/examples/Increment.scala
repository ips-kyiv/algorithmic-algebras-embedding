package schemas.examples

import ua.ips.algo.{given,*} 

object Increment extends SchemaProvider:

   val defaultSchema = Schema.build{
     (x:Int) => x+1
   }


