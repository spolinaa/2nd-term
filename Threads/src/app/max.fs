(* Max Finder in array (parallel)
(expectation: 2 h, reality: 1,5 h)
by Sokolova Polina
*)

module Max

open System.Threading

exception EmptyArray

let maxInRange (a : int []) l r =
  let mutable max = 0
  for i in l .. r do
    if a.[i] > max then
      max <- a.[i]
  max

let findMax (a : int []) threadNumber =
  let step = a.Length / threadNumber
  let max = ref 0
  let threadArray = Array.init threadNumber (fun i ->
      new Thread(ThreadStart(fun _ ->
          let threadRes = maxInRange a (i * step) ((i + 1) * step - 1)
          if !max < threadRes then 
            max := threadRes
        ))
    )
  for t in threadArray do
    t.Start()
  for t in threadArray do
    t.Join()
  !max

let duration s f = 
  let timer = new System.Diagnostics.Stopwatch()
  timer.Start()
  let returnValue = f()
  printfn "%s, Elapsed Time: %i" s timer.ElapsedMilliseconds
  returnValue

let makeArray p =
  let rnd = new System.Random(0)
  let n = pown 10 p
  Array.init n (fun i -> rnd.Next(0, n))

[<EntryPoint>]
let main args =
(*Deg = 1
    Thread : 1, Elapsed Time: 170
    Thread : 2, Elapsed Time: 3
    Thread : 3, Elapsed Time: 3
    Thread : 4, Elapsed Time: 6     
    Thread : 5, Elapsed Time: 6
    Thread : 6, Elapsed Time: 5
    Thread : 7, Elapsed Time: 6
    Thread : 8, Elapsed Time: 6
    Thread : 9, Elapsed Time: 8
    Thread : 10, Elapsed Time: 10

  Deg = 2
    Thread : 1, Elapsed Time: 169
    Thread : 2, Elapsed Time: 2
    Thread : 3, Elapsed Time: 6
    Thread : 4, Elapsed Time: 3
    Thread : 5, Elapsed Time: 4
    Thread : 6, Elapsed Time: 10
    Thread : 7, Elapsed Time: 6
    Thread : 8, Elapsed Time: 112
    Thread : 9, Elapsed Time: 9
    Thread : 10, Elapsed Time: 8
    Thread : 11, Elapsed Time: 8
    Thread : 12, Elapsed Time: 14
    Thread : 13, Elapsed Time: 11
    Thread : 14, Elapsed Time: 11
    Thread : 15, Elapsed Time: 117
    Thread : 16, Elapsed Time: 12
    Thread : 17, Elapsed Time: 15
    Thread : 18, Elapsed Time: 15
    Thread : 19, Elapsed Time: 16
    Thread : 20, Elapsed Time: 14

  Deg = 3
    Thread : 1, Elapsed Time: 173
    Thread : 2, Elapsed Time: 2
    Thread : 3, Elapsed Time: 104
    Thread : 4, Elapsed Time: 3
    Thread : 5, Elapsed Time: 13
    Thread : 6, Elapsed Time: 8
    Thread : 7, Elapsed Time: 6
    Thread : 8, Elapsed Time: 6
    Thread : 9, Elapsed Time: 8
    Thread : 10, Elapsed Time: 8
    Thread : 11, Elapsed Time: 10
    Thread : 12, Elapsed Time: 10
    Thread : 13, Elapsed Time: 11
    Thread : 14, Elapsed Time: 12
    Thread : 15, Elapsed Time: 17
    Thread : 16, Elapsed Time: 11
    Thread : 17, Elapsed Time: 15
    Thread : 18, Elapsed Time: 15
    Thread : 19, Elapsed Time: 15
    Thread : 20, Elapsed Time: 16

  Deg = 4
    Thread : 1, Elapsed Time: 174
    Thread : 2, Elapsed Time: 2
    Thread : 3, Elapsed Time: 3
    Thread : 4, Elapsed Time: 4
    Thread : 5, Elapsed Time: 4
    Thread : 6, Elapsed Time: 12
    Thread : 7, Elapsed Time: 6
    Thread : 8, Elapsed Time: 12
    Thread : 9, Elapsed Time: 7
    Thread : 10, Elapsed Time: 12
    Thread : 11, Elapsed Time: 113
    Thread : 12, Elapsed Time: 11
    Thread : 13, Elapsed Time: 9
    Thread : 14, Elapsed Time: 9
    Thread : 15, Elapsed Time: 14
    Thread : 16, Elapsed Time: 14
    Thread : 17, Elapsed Time: 12
    Thread : 18, Elapsed Time: 18
    Thread : 19, Elapsed Time: 17
    Thread : 20, Elapsed Time: 16

  Deg = 5
    Thread : 1, Elapsed Time: 173
    Thread : 2, Elapsed Time: 5
    Thread : 3, Elapsed Time: 4
    Thread : 4, Elapsed Time: 3
    Thread : 5, Elapsed Time: 4
    Thread : 6, Elapsed Time: 8
    Thread : 7, Elapsed Time: 6
    Thread : 8, Elapsed Time: 6
    Thread : 9, Elapsed Time: 7
    Thread : 10, Elapsed Time: 8
    Thread : 11, Elapsed Time: 10
    Thread : 12, Elapsed Time: 9
    Thread : 13, Elapsed Time: 11
    Thread : 14, Elapsed Time: 12
    Thread : 15, Elapsed Time: 12
    Thread : 16, Elapsed Time: 12
    Thread : 17, Elapsed Time: 15
    Thread : 18, Elapsed Time: 15
    Thread : 19, Elapsed Time: 18
    Thread : 20, Elapsed Time: 35  

  Deg = 6
    Thread : 1, Elapsed Time: 161
    Thread : 2, Elapsed Time: 111
    Thread : 3, Elapsed Time: 6
    Thread : 4, Elapsed Time: 6
    Thread : 5, Elapsed Time: 7
    Thread : 6, Elapsed Time: 7
    Thread : 7, Elapsed Time: 8
    Thread : 8, Elapsed Time: 10
    Thread : 9, Elapsed Time: 12
    Thread : 10, Elapsed Time: 14
    Thread : 11, Elapsed Time: 14
    Thread : 12, Elapsed Time: 13
    Thread : 13, Elapsed Time: 14
    Thread : 14, Elapsed Time: 14
    Thread : 15, Elapsed Time: 16
    Thread : 16, Elapsed Time: 18
    Thread : 17, Elapsed Time: 16
    Thread : 18, Elapsed Time: 18
    Thread : 19, Elapsed Time: 19
    Thread : 20, Elapsed Time: 17

  Deg = 7
    Thread : 1, Elapsed Time: 186
    Thread : 2, Elapsed Time: 28
    Thread : 3, Elapsed Time: 33
    Thread : 4, Elapsed Time: 37
    Thread : 5, Elapsed Time: 40
    Thread : 6, Elapsed Time: 37
    Thread : 7, Elapsed Time: 38
    Thread : 8, Elapsed Time: 147
    Thread : 9, Elapsed Time: 40
    Thread : 10, Elapsed Time: 42
    Thread : 11, Elapsed Time: 42
    Thread : 12, Elapsed Time: 45
    Thread : 13, Elapsed Time: 44
    Thread : 14, Elapsed Time: 47
    Thread : 15, Elapsed Time: 47
    Thread : 16, Elapsed Time: 51
    Thread : 17, Elapsed Time: 157
    Thread : 18, Elapsed Time: 49
    Thread : 19, Elapsed Time: 51
    Thread : 20, Elapsed Time: 51      

  Deg = 8
    Thread : 1, Elapsed Time: 989
    Thread : 2, Elapsed Time: 249
    Thread : 3, Elapsed Time: 429
    Thread : 4, Elapsed Time: 438
    Thread : 5, Elapsed Time: 323
    Thread : 6, Elapsed Time: 362
    Thread : 7, Elapsed Time: 397
    Thread : 8, Elapsed Time: 381
    Thread : 9, Elapsed Time: 337
    Thread : 10, Elapsed Time: 376
    Thread : 11, Elapsed Time: 340
    Thread : 12, Elapsed Time: 405
    Thread : 13, Elapsed Time: 398
    Thread : 14, Elapsed Time: 355
    Thread : 15, Elapsed Time: 480
    Thread : 16, Elapsed Time: 440
    Thread : 17, Elapsed Time: 363
    Thread : 18, Elapsed Time: 335
    Thread : 19, Elapsed Time: 346
    Thread : 20, Elapsed Time: 326 *)
  let a = makeArray 3
  duration (sprintf "Thread : 1") (fun () -> printfn "%A" (findMax a 1))
  duration (sprintf "Thread : 2") (fun () -> printfn "%A" (findMax a 2))
  duration (sprintf "Thread : 3") (fun () -> printfn "%A" (findMax a 3))
  duration (sprintf "Thread : 4") (fun () -> printfn "%A" (findMax a 4))
  duration (sprintf "Thread : 5") (fun () -> printfn "%A" (findMax a 5))
  duration (sprintf "Thread : 6") (fun () -> printfn "%A" (findMax a 6))
  duration (sprintf "Thread : 7") (fun () -> printfn "%A" (findMax a 7))
  duration (sprintf "Thread : 8") (fun () -> printfn "%A" (findMax a 8))
  duration (sprintf "Thread : 9") (fun () -> printfn "%A" (findMax a 9))
  duration (sprintf "Thread : 10") (fun () -> printfn "%A" (findMax a 10))
  duration (sprintf "Thread : 11") (fun () -> printfn "%A" (findMax a 11)) 
  duration (sprintf "Thread : 12") (fun () -> printfn "%A" (findMax a 12))
  duration (sprintf "Thread : 13") (fun () -> printfn "%A" (findMax a 13))
  duration (sprintf "Thread : 14") (fun () -> printfn "%A" (findMax a 14))
  duration (sprintf "Thread : 15") (fun () -> printfn "%A" (findMax a 15))
  duration (sprintf "Thread : 16") (fun () -> printfn "%A" (findMax a 16))
  duration (sprintf "Thread : 17") (fun () -> printfn "%A" (findMax a 17))
  duration (sprintf "Thread : 18") (fun () -> printfn "%A" (findMax a 18))
  duration (sprintf "Thread : 19") (fun () -> printfn "%A" (findMax a 19))
  duration (sprintf "Thread : 20") (fun () -> printfn "%A" (findMax a 20))

  0