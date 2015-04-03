(* Tests for tasks 28 - 29
(expectation: 3 h; reality: 5,5 h)
by Sokolova Polina *)


module t1

open System
open NUnit.Framework
 
type IGraph<'A> =
  interface
    abstract IsEdge    : int  -> int -> bool
    abstract NumberOfV : unit -> int
    abstract NumberOfE : unit -> int
  end

type ArrayGraph<'A> (array : bool [,]) =
  class
    let a = array
    let n = Array2D.length1 array 

    interface IGraph<'A> with
      member s.IsEdge p q = a.[p, q]
      member s.NumberOfV () = n
      member s.NumberOfE () =
        let mutable acc = 0
        for i = 0 to (n - 1) do
          for j = 0 to (n - 1) do
            if a.[i,j] then acc <- acc + 1
        acc
  end

type ListGraph<'A> (list : int list []) =
  class
    let l = list
    let n = list.Length 

    interface IGraph<'A> with
      member s.IsEdge p q =
        let f = fun acc ver -> if ver = q then acc + 1
                                          else acc
        let fold = List.fold (f) 0 l.[p]
        if (fold = 0) then false
        else true
      member s.NumberOfV () = n
      member s.NumberOfE () =
        Array.fold (fun acc e -> acc + List.length e) 0 l 
  end
   
let to_ (graph : IGraph<'A>, a) =
  let n = graph.NumberOfV ()
  if (a < n) && (a >= 0) then
    let array = Array.create n 0
    let mutable res = []
    array.[a] <- 1
    let rec wayTo k =
      for i = 0 to (n - 1) do
        if (graph.IsEdge k i) && (array.[i] = 0) 
        then array.[i] <- 1; wayTo i 
    wayTo a
    array.[a] <- 0
    for i = (n - 1) downto 0 do
      if (array.[i] = 1) then res <- i :: res
    res
  else []

let from_ (graph : IGraph<'A>, a) =
  let n = graph.NumberOfV ()
  if (a < n) && (a >= 0) then
    let array = Array.create n 0
    let mutable res = []
    array.[a] <- 1
    let rec wayFrom k =
      for i = 0 to (n - 1) do
        if (graph.IsEdge i k) && (array.[i] = 0) 
          then array.[i] <- 1; wayFrom i
    wayFrom a
    array.[a] <- 0
    for i = (n - 1) downto 0 do
      if (array.[i] = 1) then res <- i :: res
    res
  else []

[<Test>]
let to_emptyArray () =
  let a = Array2D.create 0 0 false
  let graph = new ArrayGraph<int> (a) :> IGraph<int>
  Assert.AreEqual (to_ (graph, 0), [])

[<TestFixture>]
type ``Out of index (to)`` () =
  let a = Array2D.create 4 4 false
  let graph = new ArrayGraph<int> (a) :> IGraph<int>

  [<Test>]
  member x.``Index 7 of 4`` () =
    Assert.AreEqual (to_ (graph, 7), []) 

  [<Test>]
  member x.``Index -2 of 4`` () =
    Assert.AreEqual (to_ (graph, -2), [])

[<TestFixture>]
type ``Ways from a vertice`` () =
  let a = Array2D.create 4 4 false

  let graph = new ArrayGraph<int> (a) :> IGraph<int>

  [<Test>]
  member x.``Ways from 0`` () =
    Array2D.set a 0 1 true
    Array2D.set a 0 2 true
    Array2D.set a 3 0 true
    Assert.AreEqual (to_ (graph, 0), [1; 2])

  [<Test>]
  member x.``Ways from 4`` () =
    Array2D.set a 0 1 true
    Array2D.set a 0 2 true
    Array2D.set a 3 0 true
    Assert.AreEqual (to_ (graph, 4), [])

[<Test>]
let from_emptyArray () =
  let a = Array2D.create 0 0 false
  let graph = new ArrayGraph<int> (a) :> IGraph<int>
  Assert.AreEqual (from_ (graph, 0), [])

[<TestFixture>]
type ``Out of index (from)`` () =
  let a = Array2D.create 4 4 false
  let graph = new ArrayGraph<int> (a) :> IGraph<int>

  [<Test>]
  member x.``Index 7 of 4`` () = 
    Assert.AreEqual (from_ (graph, 7), []) 

  [<Test>]
  member x.``Index -2 of 4`` () =
    Assert.AreEqual (from_ (graph, -2), [])

[<Test>]
let from_edgeYes () =
  let a = Array2D.create 4 4 false
  Array2D.set a 0 1 true
  Array2D.set a 0 2 true
  Array2D.set a 3 2 true
  let graph = new ArrayGraph<int> (a) :> IGraph<int>
  Assert.AreEqual (from_ (graph, 2), [0; 3])

[<Test>]
let from_edgeNo () =
  let a = Array2D.create 4 4 false
  Array2D.set a 0 1 true
  Array2D.set a 0 2 true
  Array2D.set a 0 3 true
  let graph = new ArrayGraph<int> (a) :> IGraph<int>
  Assert.AreEqual (from_ (graph, 0), [])

[<EntryPoint>]
let main argv = 
  0
