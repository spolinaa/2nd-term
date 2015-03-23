(* Model of local network
(expectation: 3 h; reality: 3 h)
by Sokolova Polina *)

type IGraph =
  interface
    abstract IsEdge    : int  -> int -> bool
    abstract NumberOfV : unit -> int
  end

type IMarkedGraph<'A> =
  interface
    inherit IGraph

    abstract ToMark : int -> 'A
  end

type ArrayGraph (array : bool [,]) =
    class
        let a = array
        let n = Array2D.length1 array 

        interface IGraph with
            member s.IsEdge p q = 
                a.[p, q]
            member s.NumberOfV () = n
    end

let probability os =
  match os with
  | "OS X"    -> 0.1
  | "Linux"   -> 0.3
  | "Windows" -> 0.7
  | _         -> failwith "Not supported OS!"


type Computer (os : string, infected : bool, number : int) =
  class
    let mutable i = infected
    
    member s.IsInfected = i
    member s.Infection (var : float) =
      if not i then if var <= probability os then i <- true
    member s.State =
      printf "%A (%A)" os number
      if s.IsInfected then printfn " + " else printfn " - "
  end

type LocalNetwork (os : string [], connected : bool [,], infection : bool []) =
  class
    inherit ArrayGraph (connected)
    let a = connected
    let c = [|for i in [0 .. infection.Length - 1] -> new Computer(os.[i], infection.[i], i)|]
    let mutable count = 1

    member s.LetsInfect =
      let n = infection.Length - 1
      let bad = Array.filter (fun i -> c.[i].IsInfected) [|0 .. n|]
      for i in bad do
        for k = 0 to n do
          if a.[i, k] then 
            let r = System.Random().NextDouble()
            let reaction = c.[k].IsInfected
            c.[k].Infection r
        printfn "\nStep %A:" count 
        for j = 0 to n do
          c.[j].State      
        count <- count + 1
          

    interface IMarkedGraph<string> with
      member s.ToMark number = os.[number]    
  end

[<EntryPoint>]
let main args =
  let OS = [|"OS X"; "OS X"; "OS X"; "Linux"; "Linux"; "Windows"; "Windows"|]
  let infect = [|false; false; false; true; false; false; true|]
  let array = Array2D.create 7 7 false // Вершины 0 - 6
  Array2D.set array 0 1 true
  Array2D.set array 1 4 true
  Array2D.set array 2 4 true
  Array2D.set array 2 5 true
  Array2D.set array 2 6 true
  Array2D.set array 3 4 true
  Array2D.set array 3 5 true
  Array2D.set array 5 6 true
  Array2D.set array 1 0 true
  Array2D.set array 4 1 true
  Array2D.set array 4 2 true
  Array2D.set array 5 2 true
  Array2D.set array 6 2 true
  Array2D.set array 4 3 true
  Array2D.set array 5 3 true
  Array2D.set array 6 5 true
  Array2D.set array 0 6 true
  Array2D.set array 6 0 true
  printfn "       MyNetwork:\n  
  OS X (0)  -  OS X (1)
                 | 
 Linux (3)  -  Linux (4)
   |             |
Windows (5) -  OS X (2)
            |
       Windows (6)\n" 
  let MyNetwork = new LocalNetwork (OS, array, infect)
  let c = [|for i in [0 .. infect.Length - 1] -> new Computer(OS.[i], infect.[i], i)|]
  for i = 0 to (infect.Length - 1) do
    c.[i].State
  for i = 0 to 6 do
    MyNetwork.LetsInfect
  0
  
