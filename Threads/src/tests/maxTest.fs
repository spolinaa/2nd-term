module MaxTest

open Max
open NUnit.Framework

[<TestCase(1, 1, Result = 9)>]
[<TestCase(2, 7, Result = 99)>]
[<TestCase(3, 3, Result = 999)>]
[<TestCase(4, 6, Result = 9999)>]
[<TestCase(5, 4, Result = 99998)>]
[<TestCase(6, 9, Result = 999999)>]
[<TestCase(7, 8, Result = 9999999)>]
[<TestCase(8, 2, Result = 99999998)>]

[<TestCase(1, 4,  Result = 9)>]
[<TestCase(2, 10, Result = 99)>]
[<TestCase(3, 18, Result = 999)>]
[<TestCase(4, 1,  Result = 9999)>]
[<TestCase(5, 9,  Result = 99998)>]
[<TestCase(6, 2,  Result = 999999)>]
[<TestCase(7, 3,  Result = 9999999)>]
[<TestCase(8, 7,  Result = 99999998)>]
let ``Find max in array`` n t =
  findMax (makeArray n) t 

