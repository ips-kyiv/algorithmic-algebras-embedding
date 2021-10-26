package ua.ips.algo.translation.languages.clike

import ua.ips.algo.*
import ua.ips.algo.translation.*


/**
 * Mangling of names between C and Schema
 **/
object NamesMangling {

  def objModuleName(signature: DataSortSignature, variant: Seq[String]): String = {
    // TODO: implement manginglin with all types of parameters and hash of variant.
    "main";
  }


  def objFunctionName(signature: DataSortSignature, variant: Seq[String]): String = {
    // TODO: implement manginglin with all types of parameters and hash of variant.
    signature.name;
  }

}