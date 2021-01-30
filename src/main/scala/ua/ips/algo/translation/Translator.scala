package ua.ips.algo.translation

import ua.ips.algo._

class Translator(val target: Target)
{

   def compile(input: Schema): target.language.OutputBundle =
      
      val optimized = optimize(input)
      codeGen(optimized)
      
   

   def optimize(input: Schema): Schema =
      ???
      
   def codeGen(input: Schema): target.language.OutputBundle =
      ??? 
    
}





