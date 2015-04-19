(* Workflow for tree
(expectation: 5 h; reality: 5,5 h)
by Sokolova Polina *)

module TreeWorkflow

type Tree = Nil | Node of int * Tree * Tree
  
type TreeBuilder () =
  member x.Bind (a, f) = 
    let rec func a =
      match a with
      | Nil -> Nil
      | Node(c, L, R) -> Node(f c, func L, func R)
    func a
  member x.Return a = Node(a, Nil, Nil)
  member x.ReturnFrom a = a
  member x.Combine (t1, t2) = 
    let rec merge t1 t2 =
      match (t1, t2) with
      | Nil, Nil -> Nil
      | Node(c, L, R), Nil -> t1
      | Nil, Node(c, L, R) -> t2
      | Node(c1, L1, R1), Node(c2, L2, R2) ->
        if c1 > c2 then Node(c1, L1, merge R1 t2)
        else Node(c2, merge t1 L2, R2)
    merge t1 t2
  member x.For (a, f) = x.Bind (a, f)
  member x.Delay f = f ()

let tree = new TreeBuilder ()

let map f t =
  tree {
    for x in t do
      return! f x 
  }

let rec fold f t state =
  tree {
    match t with
    | Nil -> return! state
    | Node(c, L, R) -> return! fold f R (fold f L (f c state))
  }

let rec filter f t =
  tree {
    match t with
    | Nil -> return! Nil
    | Node(c, L, R) -> if (f c) then return! Node(c, filter f L, filter f R)
                       else return! filter f L
  }

[<EntryPoint>]
let main argv = 
  let t1 = Node(9, Node(1, Nil, Nil), Nil)
  let t2 = Node(5, Node(3, Nil, Nil), Node(11, Nil, Nil))
  let t3 = Node(9, Node(7, Node(5, Node(1, Nil, Nil), Nil), Nil), Node(11, Nil, Nil))
  printfn "%A" (tree.Combine (t1, t2))
  0 
