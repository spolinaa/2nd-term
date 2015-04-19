module Calc

open Parser

exception NotInitializedVariable of string

let rec find_var i (var : string) (array : string []) =
      if i < array.Length then
        match array.[i] with
        | x when array.[i] = var -> i
        | _                      -> find_var (i + 1) var array 
      else -1

let no_some (x : Option<int>, v : string) =
  match x with
  | None   -> raise (NotInitializedVariable v)
  | Some a -> a

let return_arg (x : Option<int>, v : string) =
  try no_some (x, v) with
  | :? NotInitializedVariable -> printfn "Uninitialized variable %A" v; 0

let rec calculate exp (val_a : Option<int> []) var_array : int =
      match exp with
      | Num n -> n
      | Var a -> let k = find_var 0 a var_array
                 return_arg (val_a.[k], a)             
      | BinOp(a, b, c) -> match a with
                           | '+' -> calculate b val_a var_array + calculate c val_a var_array
                           | '-' -> calculate b val_a var_array - calculate c val_a var_array
                           | '*' -> calculate b val_a var_array * calculate c val_a var_array
                           | '%' -> calculate b val_a var_array % calculate c val_a var_array
                           | '/' -> calculate b val_a var_array / calculate c val_a var_array
                           | '^' -> let mutable res = 1
                                    for i = 1 to calculate c val_a var_array do
                                      res <- calculate b val_a var_array * res
                                    res
                           | _   -> raise (invalidOp ("Unknown operation"))

let calc exp (val_a : Option<int> []) var_array =
  let operation =
    match exp with
    | BinOp(a, b, c) -> a
    | _              -> '0'
  try calculate exp val_a var_array with
  | invalidOp -> printfn "Unknown operation %A" operation; 0




