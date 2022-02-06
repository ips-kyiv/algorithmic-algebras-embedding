package embedded


import ua.ips.algo._
import ua.ips.algo.given

import munit._

class ParseMinimalSchemaTest extends FunSuite {


  test("parseMinimalSchema") {

    val schema = Schema.build{
      (x:Int) => x+1
    }
    println(schema)
    assert(true)

  }

  test("parseCondition") {

    val schema = Schema.build{
      (x:Int) => if (x > 0) 1 else 0
    }
    println(schema)
    schema match
      case SequentialSchema(
              InputSchema(params,pos1),
              ConditionalSchema(c,x,y,pos2),
              pos0
           ) =>
         assert(true)
      case _ =>
         assert(false)

  }

  test("parseLoop") {

    val schema = Schema.build{
      (x:Int) =>
          var y = x
          while(y > 0) {
            y = y - 1
          }
    }


  }

  test("parseParLoop") {
    val schema = Schema.build{
      (x:Int) =>
        var y = x
        for(a <- (1 to 4).par(4)) {
           // TODO: tread_index
          // TODO: atomit{
            y = y + a
          //}
        }
    } 
    println(schema)
  }

  test("parseBinaryOp") {
    val schema = Schema.build{
      (x:Int, y:Int) => x + y
    } 
    println(schema)
  }

  test("minArray") {
      var schema = Schema.build{
      (arr: Array[Int]) => 
        var min = Int.MaxValue
        for (v <- arr){
          if (v < min){
            min = v
          }
        }
      }
      println(schema)
  }

  // test("bubbleSorting") {
  //   var schema = Schema.build{
  //     (arr: Array[Int]) =>
  //       for (i <- 1 to arr.length - 1){
  //         for (j <- (i-1) to 0 by -1){
  //           if (arr(j) > arr(j + 1)){
  //             val temp = arr(j + 1)
  //             arr(j + 1) = arr(j)
  //             arr(j) = temp
  //           }
  //         }
  //       }
  //   }
  // }
}
