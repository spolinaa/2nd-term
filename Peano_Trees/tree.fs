 (* Some operations (insert, remove, print) with binary search trees
  (expectation: 4 h; reality: 4 h)
  by Sokolova Polina *)

type Tree = Nil | Node of int * Tree * Tree

let rec insert a t =
match t with
| Nil           -> Node(a, Nil, Nil)
| Node(c, L, R) -> match compare a c with
| x when x < 0 -> Node(c, insert a L, R)
| x when x > 0 -> Node(c, L, insert a R)
| _            -> t

let rec findInRight t =
match t with
| Nil             -> 0
| Node(c, Nil, _) -> c
| Node(_, L  , _) -> findInRight L

let rec findInLeft t =
match t with
| Nil             -> 0
| Node(c, _, Nil) -> c
| Node(c, _, R  ) -> findInLeft R

let rec remove a t =
match t with
| Nil           -> Nil
| Node(c, L, R) -> match compare a c with
| x when x < 0 -> Node(c, remove a L, R)
| x when x > 0 -> Node(c, L, remove a R)
| _            -> match (L, R) with
| Nil, Nil             -> Nil
| L  , Nil             -> L
| Nil, R               -> R
| L  , Node(_, Nil, _) -> Node(findInLeft L, remove (findInLeft L) L, R)
| _  , _               -> Node(findInRight R, L, remove (findInRight R) R)

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
let myTree = Node (8,Node(3,Node(1,Nil,Nil),Node(6,Node(4,Nil,Nil),Node(7,Nil,Nil))),Node(10,Nil,(Node(14,Node(13,Nil,Nil),Nil))))
printf "%A\n" myTree
printf "LCR: "
LCRprint myTree
printf "\nLRC: "
LRCprint myTree
printf "\nCLR: "
CLRprint myTree
printf "\n"
let myTree = remove 8 myTree
printf "remove 8: %A\n" myTree
let myTree = remove 7 myTree
printf "remove 7: %A\n" myTree
let myTree = insert 9 myTree
printf "insert 9: %A\n" myTree
let myTree = insert 11 myTree
printf "insert 11: %A\n" myTree
let myTree = remove 6 myTree
printf "remove 6: %A\n" myTree
let myTree = remove 70 myTree
printf "remove 70: %A\n" myTree
printf "%A\n" myTree
printf "LCR: "
LCRprint myTree
printf "\nLRC: "
LRCprint myTree
printf "\nCLR: "
CLRprint myTree
printf "\n"
0