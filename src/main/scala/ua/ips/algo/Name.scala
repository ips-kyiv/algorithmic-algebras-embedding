package ua.ips.algo


//TODO: this should be sequence of strings.
type Name = String


case class FullName(packageName: Seq[String], name: String) 

object FullName {

  def fromString(fullName: String):FullName = {
    val arr = fullName.split("\\.")
    FullName(arr.slice(0,arr.length-1),arr(arr.length-1))
  } 

}