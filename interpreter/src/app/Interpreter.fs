module Interpreter

open Parser 
open System.IO

exception NotInitializedVariable of string

let inter (s : string [], values_list : int [],  where_print : bool) =
  let mutable i = 0
  let rec find i count =
    if i < s.Length then
      match s.[i] with
      | "while" -> if count = 0 then i
                   else find (i + 1) count
      | "if"    -> if count = 0 then i
                   else find (i + 1) (count + 1)
      | ":="| "write"| "read" -> if count = 0 then i
                                 else find (i + 1) (count - 1)
      | ";" -> if count = 0 then i
               else find (i + 1) (count + 1) 
                 

      | _   -> find (i + 1) count
    else 0

  let listOfVar = ref [||]

  let rec find_var i (var : string) =
      if i < (!listOfVar).Length then
        match (!listOfVar).[i] with
        | x when (!listOfVar).[i] = var -> i
        | _                  -> find_var (i + 1) var 
      else -1

  let rec read_exp i =
    match s.[i] with
    | x when x >= "a" && x <= "z" || x >= "A" && x <= "Z" -> 
      if find_var 0 s.[i] < 0 
      then listOfVar := Array.append !listOfVar [|s.[i]|] 
      Var s.[i]
    | x when x >= "0" && x <= "9" -> Num (int s.[i])
    | op -> BinOp (char op, read_exp (i + 1), read_exp (i + 2))

  let rec read i : Tree =
    if i < s.Length then 
      match s.[i] with
      | ";"  -> let k = find (i + 1) 0
                if k <> 0 then 
                  let k2 =
                    if s.[k] = "while" then 
                      find (k + 1) 1
                    elif s.[k] = "if" then find (k + 1) 2
                    else find (k + 1) 0
                  Node(read k, read k2)
                else read (i + 1)
      | ":=" -> let k = read_exp (i + 1)
                Assign(s.[i + 1], read_exp (i + 2))          
      | "if" -> let k = find (i + 1) 0
                if k <> 0 then
                  let k2 =
                    if s.[k] = "while" then 
                      find (k + 1) 1
                    elif s.[k] = "if" || s.[k] = ";" then find (k + 1) 2
                    else find (k + 1) 0
                  if k2 <> 0 then 
                    If(read_exp (i + 1), read k, read k2)
                  else If(read_exp (i + 1), read k, Nil)
                else If(read_exp (i + 1), Nil, Nil)
      | "while" -> let k = find (i + 1) 0 
                   if k <> 0 then
                     While(read_exp (i + 1), read k)
                   else While(read_exp (i + 1), Nil)    
      | "read" -> let k = read_exp (i + 1) 
                  Read s.[i + 1]
      | "write" -> Write(read_exp (i + 1)) 
      | _   -> read (i + 1)
    else Nil

  let myTree = read 0 
  let var_array = !listOfVar // make an array of variables

  let program (value_array : int [], var_array : string []) t = // execute all commands 
    let rec find_var i (var : string) =
      if i < var_array.Length then
        match var_array.[i] with
        | x when var_array.[i] = var -> i
        | _                          -> find_var (i + 1) var 
      else 0

    let no_some (x : Option<int>, v : string) =
      match x with
      | None   -> raise (NotInitializedVariable v)
      | Some a -> a

    let return_arg (x : Option<int>, v : string) =
      try no_some (x, v) with
      | :? NotInitializedVariable -> printfn "Uninitialized variable %A" v; 0

    let rec calculate exp (val_a : Option<int> []) : int =
      match exp with
      | Num n -> n
      | Var a -> let k = find_var 0 a
                 return_arg (val_a.[k], a)             
      | BinOp(a, b, c) -> match a with
                           | '+' -> calculate b val_a + calculate c val_a
                           | '-' -> calculate b val_a - calculate c val_a
                           | '*' -> calculate b val_a * calculate c val_a
                           | '%' -> calculate b val_a % calculate c val_a
                           | '/' -> calculate b val_a / calculate c val_a
                           | '^' -> let mutable res = 1
                                    for i = 1 to calculate c val_a do
                                      res <- calculate b val_a * res
                                    res
                           | _   -> raise (invalidOp ("Unknown operation"))

    let calc exp (val_a : Option<int> []) =
      let operation =
        match exp with
        | BinOp(a, b, c) -> a
        | _              -> '0'
      try calculate exp val_a with
      | invalidOp -> printfn "Unknown operation %A" operation; 0

    use stream = new StreamWriter("output.txt") 
    let write agree (res : 'A) =
      let write_in text =
        stream.WriteLine(res)
      match agree with
      | true  -> write_in res
      | false -> printfn "%A" res

    let mutable val_a = Array.create var_array.Length None
    let size = ref -1

    let rec action tree val_ar =
      match tree with
      | Node(t1, t2)    -> action t1 val_ar; action t2 val_ar 
      | If(exp, t1, t2) -> if (calc exp val_ar > 0) then action t1 val_ar
                           else action t2 val_ar
      | While(exp, t)   -> while (calc exp val_ar > 0) do
                             action t val_ar
      | Read str        -> size := !size + 1
                           let k = find_var 0 str
                           if !size >= value_array.Length then 
                             printfn "Enter %A" str
                             val_ar.[k] <- Some (int (System.Console.ReadLine()))
                           else val_ar.[k] <- Some value_array.[!size];              
      | Write exp       -> write where_print (calc exp val_ar)
      | Assign(s, exp)  -> let k = find_var 0 s
                           val_ar.[k] <- Some (calc exp val_ar)
      | _               -> () 

    action t val_a
  program (values_list, var_array) myTree

