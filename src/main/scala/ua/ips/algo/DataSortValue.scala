package ua.ips.algo

case class DataSortValue[T](sortRep: DataSortRep[T], value: T) {
  type V = T

  def constantFor(interpreter: Interpretation)(scope: interpreter.DataScope): interpreter.DataItem =
    interpreter.constant(scope, value, sortRep)

}

object DataSortValue {

  

}