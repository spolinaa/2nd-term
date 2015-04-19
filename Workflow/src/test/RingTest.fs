module RingTest

open RingWorkflow
open NUnit.Framework

[<TestCase(5, Result = 0)>]
[<TestCase(14, Result = 10)>] 
[<TestCase(1, Result = 0)>]
[<TestCase(9, Result = 1)>]
[<TestCase(7, Result = 3)>]
let ``2 * 3 + 4`` n =
  ring n {
    let! a = 2 * 3
    let! b = 4
    return a + b
  }

[<TestCase(7, Result = 0)>]
[<TestCase(643, Result = 1)>]
[<TestCase(11, Result = 6)>]
let ``4 * 7 * (17 + 6)`` n =
  ring n {
    let! a = 4 * 7
    let! b = 17 + 6
    let! c = a * b
    return c
  }

[<TestCase(3, Result = 2)>]
[<TestCase(7, Result = 0)>]
[<TestCase(10, Result = 3)>]
let ``-7`` n =
  ring n {
    let! a = -7
    return a
  } 

