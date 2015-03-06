(* Some functions (reverse, filter, map, horner's method) for list
(expectation: 2,5 h; reality: 3 h)
by Sokolova Polina *)

//List.iter : ('a -> unit) -> 'a list -> unit

let reverse l = List.fold (fun acc node -> node :: acc) [] l 

let filter f l =
    List.fold (fun acc node -> if f node then node :: acc
                               else acc) [] (reverse l)
let map f l = 
    List.fold (fun acc node -> f node :: acc) [] (reverse l)

let horner l x =
    (List.fold (fun acc num -> (num + acc) * x) 0 (reverse l)) / x

[<EntryPoint>]
let main args =
    let myList = [2; 5; 0; 9; 1; 5; 3; 7; 8]
    let x = 2
    printfn "myList:          %A" myList
    printfn "reverse:         %A" (reverse myList)
    printfn "filter (x > 4):  %A" (filter (fun x -> x > 4) myList)
    printfn "map (x * 2 + 1): %A" (map (fun x -> x * 2 + 1) myList)
    printfn "horner: %A" (horner myList x) 
    0