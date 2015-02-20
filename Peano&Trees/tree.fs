(* Some operations (insert, remove, print) with binary search trees
(expectation: 4 h; reality: 3,5 h)
by Sokolova Polina *)

type Tree = Nil | Node of int * Tree * Tree

let rec insert a t =
    match t with
    | Nil           -> Node(a, Nil, Nil)
    | Node(c, L, R) -> if a = c then t
                       elif a < c then Node(c, insert a L, R)
                       else Node(c, L, insert a R)

let rec findInRight t =
    match t with
    | Nil -> 0
    | Node(c, Nil, R) -> c
    | Node(c, L  , R) -> findInRight L
 
let rec findInLeft t =
    match t with
    | Nil -> 0
    | Node(c, L, Nil) -> c
    | Node(c, L, R  ) -> findInLeft R

let rec remove a t =
    match t with
    | Nil           -> Nil
    | Node(c, L, R) -> if a < c then Node(c, remove a L, R)
                       elif a > c then Node(c, L, remove a R)
                       else match (L, R) with 
                            | Nil, Nil -> Nil
                            | L  , Nil -> L
                            | Nil, R   -> R
                            | L  , R   -> if L = Nil then Node(findInRight R, L, remove (findInRight R) R)
                                          else Node(findInLeft L, remove (findInLeft L) L, R)
let rec CLRprint t =
    match t with
    | Nil           -> printf ""
    | Node(c, L, R) -> printf "%d " c
                       CLRprint L 
                       CLRprint R

let rec LCRprint t =
    match t with
    | Nil           -> printf ""
    | Node(c, L, R) -> LCRprint L
                       printf "%d " c
                       LCRprint R

let rec LRCprint t =
    match t with
    | Nil           -> printf ""
    | Node(c, L, R) -> LRCprint L
                       LRCprint R
                       printf "%d " c


[<EntryPoint>]
let main args =
    let myTree = Node(8, Node(3, Node(1, Nil, Nil), Node(6, Node(4, Nil, Nil), Node(7, Nil, Nil))), Node(10, Nil, (Node(14, Node(13, Nil, Nil), Nil))))
    let myTree = remove 8 myTree
    LCRprint myTree
    printf "\n"
    let myTree = remove 7 myTree
    LCRprint myTree
    printf "\n"
    let myTree = insert 9 myTree
    LCRprint myTree
    printf "\n"
    let myTree = remove 6 myTree
    LCRprint myTree
    printf "\n"
    let myTree = remove 70 myTree
    LCRprint myTree
    printf "\n"
