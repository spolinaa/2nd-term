(* Tests for tasks 28 - 29
(expectation: 4 h; reality: 4 h)
by Sokolova Polina *)

module t2
open System
open NUnit.Framework

type IGraph =
  interface
    abstract IsEdge    : int  -> int -> bool
    abstract NumberOfV : unit -> int
  end

type ArrayGraph (array : bool [,]) =
  class
    let a = array
    let n = Array2D.length1 array 

    interface IGraph with
      member s.IsEdge p q = a.[p, q]
      member s.NumberOfV () = n
  end

let probability os =
  match os with
  | "OS X"    -> 0.1
  | "Linux"   -> 0.3
  | "Windows" -> 0.6
  | _         -> failwith "Not supported OS"

type Computer (os : string, infected : bool, number : int) =
  class
    let mutable i = infected
    
    member s.IsInfected = i
    member s.Infection (var : float) =
      if not i && var <= probability os then i <- true
    member s.State =
      if s.IsInfected then printf "*" else printf " "
  end

type LocalNetwork (os : string [], connected : bool [,], infection : bool []) =
  class
    inherit ArrayGraph (connected)
    let a = connected
    let c = [|for i in [0 .. infection.Length - 1] -> new Computer(os.[i], infection.[i], i)|]
    let mutable count = 0

    member s.LetsInfect =
      let n = infection.Length - 1
      let bad = Array.filter (fun i -> c.[i].IsInfected) [|0 .. n|]
      for i in bad do
        for k = 0 to n do
          if a.[i, k] then 
            let r = System.Random().NextDouble()
            c.[k].Infection r
      count <- count + 1
    member s.Status  =
      printfn "\n\n       %A.MyNetwork:\n" count 
      printf "  OS X (0)" 
      c.[0].State 
      printf "  -  OS X (1)" 
      c.[1].State
      printfn  "\n                   | "
      printf " Linux (3)" 
      c.[3].State 
      printf "  -  Linux (4)" 
      c.[4].State
      printfn "\n    |              |"
      printf "Windows (5)" 
      c.[5].State 
      printf " -  OS X (2)" 
      c.[2].State
      printfn "\n             |"
      printf "       Windows (6)" 
      c.[6].State
      printfn "\n" 
  end

[<Test>] 
let ``Windows probability must be 0.6`` () =
  Assert.AreEqual(probability "Windows", 0.6)

[<Test>] 
let ``Linux probability must be 0.3`` () =
  Assert.AreEqual(probability "Linux", 0.3)

[<Test>] 
let ``OS X probability must be 0.1`` () =
  Assert.AreEqual(probability "OS X", 0.1)

//[<Test>] 
//let ``System.Exception because of unsupported OS`` () =

[<TestFixture>] 
type ``Given an infected computer`` () =
  let c = new Computer("Windows", true, 8)

  [<Test>]
  member x.``Status of infected computer must be true`` () =
    Assert.AreEqual(c.IsInfected, true)

  [<Test>] 
  member x.``Sign of infected computer is "*"`` () =
    Assert.AreEqual(c.State, printf "*")

type ``Given an uninfected computer, becoming infected`` () =

  [<Test>] 
  member x.``Status of uninfected computer must be false`` () =
    let c = new Computer("Linux", false, 8)
    Assert.AreEqual(c.IsInfected, false)

  [<Test>] 
  member x.``Sign of infected computer is "*"`` () =
    let c = new Computer("Linux", false, 8)
    c.Infection 0.2
    Assert.AreEqual(c.State, printf "*")

type ``Given an uninfected computer, cannot become infected`` () =
    
  [<Test>] 
  member x.``Status of uninfected computer must be false`` () =
    let c = new Computer("Windows", false, 8)
    Assert.AreEqual(c.IsInfected, false)

  [<Test>] 
  member x.``No sign for uninfected computer`` () =
    let c = new Computer("Windows", false, 8)
    c.Infection 0.7
    Assert.AreEqual(c.State, printf " ")

[<EntryPoint>]
let main argv = 
  0
