(* Interfaces of Graph
(expectation: 5 h; reality: 9 h)
by Sokolova Polina *)

type IGraph<'A> =
    interface
        abstract IsEdge : int -> int -> bool
        abstract NumberOfV : unit -> int
        abstract NumberOfE : unit -> int
    end

type ArrayGraph<'A> (array : bool [,]) =
    class
        let a = array
        let n = Array2D.length1 array 

        interface IGraph<'A> with
            member s.IsEdge p q = 
                if a.[p, q] then true
                else false
            member s.NumberOfV () = n
            member s.NumberOfE () =
                let mutable acc = 0
                for i = 0 to (n - 1) do
                    for j = 0 to (n - 1) do
                        if a.[i,j] then acc <- acc + 1
                        else ()
                acc
    end

type ListGraph<'A> (list : int list []) =
    class
        let l = list
        let n = list.Length 

        interface IGraph<'A> with
            member s.IsEdge p q =
                if List.fold (fun acc ver -> if ver = q then acc + 1
                                             else acc) 0 l.[p] = 0
                then false
                else true
            member s.NumberOfV () = n
            member s.NumberOfE () =
                let sum = Array.fold (fun acc e -> 
                                          acc + List.length e) 0 l 
                sum
    end
   
let To (graph : IGraph<'A>, a) =
    let n = graph.NumberOfV ()
    let array = Array.create n 0
    let mutable res = []
    let num = a
    array.[a] <- 1
    let rec wayTo k =
        for i = 0 to (n - 1) do
            if (graph.IsEdge k i) && (array.[i] = 0) 
            then array.[i] <- 1; wayTo i;  
            else ()
    wayTo a
    array.[a] <- 0
    for i = (n - 1) downto 0 do
        if (array.[i] = 1) then res <- i :: res
        else ()
    res

let From (graph : IGraph<'A>, a) =
    let n = graph.NumberOfV ()
    let array = Array.create n 0
    let mutable res = []
    let num = a
    array.[a] <- 1
    let rec wayFrom k =
        for i = 0 to (n - 1) do
            if (graph.IsEdge i k) && (array.[i] = 0) 
            then array.[i] <- 1; wayFrom i;  
            else ()
    wayFrom a
    array.[a] <- 0
    for i = (n - 1) downto 0 do
        if (array.[i] = 1) then res <- i :: res
        else ()
    res

type IMarkedGraph<'B, 'A> =
    interface
        inherit IGraph<'A>

        abstract IsEdge : 'B  -> 'B -> bool
        abstract MarkTo : 'B  -> int
        abstract ToMark : int -> 'B
    end


[<EntryPoint>]
let main args =
    let array = Array2D.create 4 4 false // Вершины 0 - 3
    Array2D.set array 0 1 true
    Array2D.set array 1 3 true
    Array2D.set array 2 0 true
    Array2D.set array 2 3 true
    Array2D.set array 3 1 true
    Array2D.set array 3 2 true
    printfn "MyGraph1: \n%A\n" array
    let MyGraph1 = new ArrayGraph<int> (array)
    printfn "Edge from 2 to 3: %A" ((MyGraph1 :> IGraph<int>).IsEdge 2 3) 
    printfn "Edge from 0 to 3: %A" ((MyGraph1 :> IGraph<int>).IsEdge 0 3)
    printfn "Number of Vertices: %A" ((MyGraph1 :> IGraph<int>).NumberOfV ())
    printfn "Number of Edges: %A\n\n" ((MyGraph1 :> IGraph<int>).NumberOfE ())

    let list = [|[1]; [0]; [0; 1; 3]; [0; 1]|]
    printfn "MyGraph2: %A\n" list
    let MyGraph2 = new ListGraph<int> (list)
    printfn "Edge from 0 to 3: %A" ((MyGraph2 :> IGraph<int>).IsEdge 0 3) 
    printfn "Edge from 2 to 1: %A" ((MyGraph2 :> IGraph<int>).IsEdge 2 1)
    printfn "Number of Vertices: %A" ((MyGraph2 :> IGraph<int>).NumberOfV ())
    printfn "Number of Edges: %A\n" ((MyGraph2 :> IGraph<int>).NumberOfE ())

    printfn "MyGraph1. From 1 to: %A" (To (MyGraph1 :> IGraph<int>, 1))
    printfn "MyGraph2. From 3 to: %A\n" (To (MyGraph2 :> IGraph<int>, 3))

    printfn "MyGraph1. To 0 from: %A" (From (MyGraph1 :> IGraph<int>, 0))
    printfn "MyGraph2. To 2 from: %A" (From (MyGraph2 :> IGraph<int>, 2))
    0