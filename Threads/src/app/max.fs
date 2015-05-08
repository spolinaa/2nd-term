(* Max Finder in array (parallel)
(expectation: 2 h, reality: 3 h)
by Sokolova Polina *)

module Max

open System.Threading

let maxInRange (a : int []) l r =
  let mutable max = a.[l]
  for i in l .. r do
    if a.[i] > max then
      max <- a.[i]
  max

let findMax (a : int []) threadNumber =
  let threadNum = ref threadNumber
  let max = ref a.[0]
  if a.Length / 2 < threadNumber then
    threadNum := a.Length / 2 
  let step = a.Length / !threadNum
  let threadArray = Array.init !threadNum (fun i ->
    new Thread(ThreadStart(fun _ ->
      let threadRes = maxInRange a (i * step) ((i + 1) * step - 1)
      lock max (fun _ -> if threadRes > !max then max := threadRes)
    ))
  )
  for t in threadArray do
    t.Start()
  for t in threadArray do
    t.Join()
  if a.[a.Length - 1] > !max then a.[a.Length - 1]
  else !max

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
(*
Deg = 1
  Thread : 1, Elapsed Time: 7
  Thread : 2, Elapsed Time: 2
  Thread : 3, Elapsed Time: 2
  Thread : 4, Elapsed Time: 4
  Thread : 5, Elapsed Time: 3
  Thread : 6, Elapsed Time: 3
  Thread : 7, Elapsed Time: 3
  Thread : 8, Elapsed Time: 4
  Thread : 9, Elapsed Time: 4
  Thread : 10, Elapsed Time: 3
  Thread : 11, Elapsed Time: 4
  Thread : 12, Elapsed Time: 4
  Thread : 13, Elapsed Time: 4
  Thread : 14, Elapsed Time: 3
  Thread : 15, Elapsed Time: 4
  Thread : 16, Elapsed Time: 4
  Thread : 17, Elapsed Time: 3
  Thread : 18, Elapsed Time: 3
  Thread : 19, Elapsed Time: 4
  Thread : 20, Elapsed Time: 9
Deg = 2
  Thread : 1, Elapsed Time: 0
  Thread : 2, Elapsed Time: 1
  Thread : 3, Elapsed Time: 3
  Thread : 4, Elapsed Time: 3
  Thread : 5, Elapsed Time: 4
  Thread : 6, Elapsed Time: 4
  Thread : 7, Elapsed Time: 6
  Thread : 8, Elapsed Time: 6
  Thread : 9, Elapsed Time: 19
  Thread : 10, Elapsed Time: 9
  Thread : 11, Elapsed Time: 8
  Thread : 12, Elapsed Time: 9
  Thread : 13, Elapsed Time: 10
  Thread : 14, Elapsed Time: 11
  Thread : 15, Elapsed Time: 12
  Thread : 16, Elapsed Time: 13
  Thread : 17, Elapsed Time: 14
  Thread : 18, Elapsed Time: 15
  Thread : 19, Elapsed Time: 31
  Thread : 20, Elapsed Time: 17
Deg = 3
  Thread : 1, Elapsed Time: 1
  Thread : 2, Elapsed Time: 2
  Thread : 3, Elapsed Time: 2
  Thread : 4, Elapsed Time: 3
  Thread : 5, Elapsed Time: 5
  Thread : 6, Elapsed Time: 4
  Thread : 7, Elapsed Time: 4
  Thread : 8, Elapsed Time: 7
  Thread : 9, Elapsed Time: 15
  Thread : 10, Elapsed Time: 12
  Thread : 11, Elapsed Time: 9
  Thread : 12, Elapsed Time: 9
  Thread : 13, Elapsed Time: 9
  Thread : 14, Elapsed Time: 20
  Thread : 15, Elapsed Time: 12
  Thread : 16, Elapsed Time: 11
  Thread : 17, Elapsed Time: 12
  Thread : 18, Elapsed Time: 14
  Thread : 19, Elapsed Time: 17
  Thread : 20, Elapsed Time: 16
Deg = 4
  Thread : 1, Elapsed Time: 1
  Thread : 2, Elapsed Time: 2
  Thread : 3, Elapsed Time: 2
  Thread : 4, Elapsed Time: 3
  Thread : 5, Elapsed Time: 4
  Thread : 6, Elapsed Time: 4
  Thread : 7, Elapsed Time: 5
  Thread : 8, Elapsed Time: 6
  Thread : 9, Elapsed Time: 7
  Thread : 10, Elapsed Time: 8
  Thread : 11, Elapsed Time: 10
  Thread : 12, Elapsed Time: 9
  Thread : 13, Elapsed Time: 21
  Thread : 14, Elapsed Time: 11
  Thread : 15, Elapsed Time: 13
  Thread : 16, Elapsed Time: 14
  Thread : 17, Elapsed Time: 14
  Thread : 18, Elapsed Time: 15
  Thread : 19, Elapsed Time: 14
  Thread : 20, Elapsed Time: 17
Deg = 5
  Thread : 1, Elapsed Time: 1
  Thread : 2, Elapsed Time: 2
  Thread : 3, Elapsed Time: 3
  Thread : 4, Elapsed Time: 4
  Thread : 5, Elapsed Time: 5
  Thread : 6, Elapsed Time: 5
  Thread : 7, Elapsed Time: 6
  Thread : 8, Elapsed Time: 6
  Thread : 9, Elapsed Time: 8
  Thread : 10, Elapsed Time: 9
  Thread : 11, Elapsed Time: 9
  Thread : 12, Elapsed Time: 9
  Thread : 13, Elapsed Time: 11
  Thread : 14, Elapsed Time: 13
  Thread : 15, Elapsed Time: 13
  Thread : 16, Elapsed Time: 12
  Thread : 17, Elapsed Time: 14
  Thread : 18, Elapsed Time: 14
  Thread : 19, Elapsed Time: 13
  Thread : 20, Elapsed Time: 16
Deg = 6
  Thread : 1, Elapsed Time: 5
  Thread : 2, Elapsed Time: 7
  Thread : 3, Elapsed Time: 5
  Thread : 4, Elapsed Time: 6
  Thread : 5, Elapsed Time: 7
  Thread : 6, Elapsed Time: 8
  Thread : 7, Elapsed Time: 12
  Thread : 8, Elapsed Time: 9
  Thread : 9, Elapsed Time: 9
  Thread : 10, Elapsed Time: 11
  Thread : 11, Elapsed Time: 13
  Thread : 12, Elapsed Time: 15
  Thread : 13, Elapsed Time: 12
  Thread : 14, Elapsed Time: 14
  Thread : 15, Elapsed Time: 15
  Thread : 16, Elapsed Time: 18
  Thread : 17, Elapsed Time: 18
  Thread : 18, Elapsed Time: 24
  Thread : 19, Elapsed Time: 16
  Thread : 20, Elapsed Time: 17
Deg = 7
  Thread : 1, Elapsed Time: 35
  Thread : 2, Elapsed Time: 22
  Thread : 3, Elapsed Time: 21
  Thread : 4, Elapsed Time: 21
  Thread : 5, Elapsed Time: 24
  Thread : 6, Elapsed Time: 25
  Thread : 7, Elapsed Time: 25
  Thread : 8, Elapsed Time: 39
  Thread : 9, Elapsed Time: 27
  Thread : 10, Elapsed Time: 31
  Thread : 11, Elapsed Time: 31
  Thread : 12, Elapsed Time: 28
  Thread : 13, Elapsed Time: 30
  Thread : 14, Elapsed Time: 37
  Thread : 15, Elapsed Time: 31
  Thread : 16, Elapsed Time: 37
  Thread : 17, Elapsed Time: 48
  Thread : 18, Elapsed Time: 35
  Thread : 19, Elapsed Time: 37
  Thread : 20, Elapsed Time: 51   
*)

  let k = ref 0
  for j = 1 to 7 do
    printfn "\nDeg = %d" j
    for i = 1 to 20 do
      let a = makeArray j
      duration (sprintf "  Thread : %A" i) (fun () -> printf "%A, " (findMax a i))
  0