module Parser

type Exp = 
  | Num of int
  | Var of string
  | BinOp of char * Exp * Exp

type Tree = 
  | Nil
  | Node of Tree * Tree
  | If of Exp * Tree * Tree
  | While of Exp * Tree
  | Write of Exp
  | Read of string
  | Assign of string * Exp

let parse (s : string) =
  let rec tokens i l exp : string list =
    if i < s.Length then
      match s.[i] with
      | '\n' -> if exp <> null then tokens (i + 1) (exp :: l) null
                else tokens (i + 1) l null
      | _    -> tokens (i + 1) l (exp + s.[i].ToString())
    elif exp <> null then (exp :: l) else l
  List.toArray(List.rev(tokens 0 [] null))

