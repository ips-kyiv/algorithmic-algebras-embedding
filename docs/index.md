## Introduction

  This is a small array DLS, based on algebraic algebras, embedded in schema.

## Types:

* PrimitiveTypes:
  * Int, Boolean, Float, Double
  * Scala mapping: appropriative Scala primitive types.
  * Language mapping: appropriative Scala primitive types.
* Tensors: (i.e. multi-dimensionals arrays)   (Not implemented yet.)
  * Scala mapping: 
    * Array[N] for one-dimensional 
    * [[ua.ips.algo.runtime.Tensor]] for many-dimensional (including one-dimensional)
  * Language mapping:  Library API
* Fixed-length arrays. (Not implemented yet)
  * Scala mapping: FixedArray[T,N]
* Named Records: set of pairs name/value where names are knonw at compile type. (Not implemented yet)
  * Scala mapping: case classes
* Tuples = Cartesian Products (Not implemented yet).
* Schemas itself (Not implemented reflective represention) -- are we need one ?
  * Scala mapping:Schema( Lambda-Functions  [TODO: object for set of schemas]


## Schema building blocks:

* Input
  * Scala representation: right part of the lambda.
  * Example:
```
    x => x+1
```
* Assignment.
  * Scala representation: value definition or assignment
  * Example
```
    val x = y + 1    
```
* Sequential Composition
  * Scala representation: sequential composition
```
    p1
    p2
```
* Parallel Composition
  * Scala representation: for loop
    * by range
```
   for(i <- (1 to 4).par) {
       ....
   }
```

    * by array, tensor or named index (not implemented)

```
   for(i <- tensor[r]) {
     ...
   }
```
* Conditional iteration
  * Scala representation: while loop
```

    while( i < 10 ) {
      i = i + 1
    }

```
* Filter (assert)

```

    assert(i < 10)

```

* Filter (if)
```
   filter(i < 10)
```
orr 
```
   if (i < 10) then
     <rest of schema> 
```

* Output
  * Scala representation: `return`  or just last expression in schema sequence.

