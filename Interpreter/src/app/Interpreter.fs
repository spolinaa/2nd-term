module Interpreter

open Parser 
open Calc
open System.IO

let mutable i = 0
let rec find i count (s : string []) =
  if i < s.Length then
    match s.[i] with
    | "while" -> if count = 0 then i
                 else find (i + 1) count s
    | "if"    -> if count = 0 then i
                 else find (i + 1) (count + 1) s
    | ":="| "write"| "read" -> if count = 0 then i
                               else find (i + 1) (count - 1) s
    | ";" -> if count = 0 then i
             else find (i + 1) (count + 1) s
    | _   -> find (i + 1) count s
  else 0

let listOfVar = ref [||]

let rec read_exp i (s : string []) =
  match s.[i] with
  | x when x >= "a" && x <= "z" || x >= "A" && x <= "Z" -> 
    if (find_var 0 s.[i] !listOfVar) < 0 
    then listOfVar := Array.append !listOfVar [|s.[i]|] 
    Var s.[i]
  | x when x >= "0" && x <= "9" -> Num (int s.[i])
  | op -> BinOp (char op, read_exp (i + 1) s, read_exp (i + 2) s)

let rec read i (s : string []) : Tree =
    if i < s.Length then 
      match s.[i] with
      | ";"  -> let k = find (i + 1) 0 s
                if k <> 0 then 
                  let k2 =
                    if s.[k] = "while" then 
                      find (k + 1) 1 s
                    elif s.[k] = "if" then find (k + 1) 2 s
                    else find (k + 1) 0 s
                  Node(read k s, read k2 s)
                else read (i + 1) s
      | ":=" -> let k = read_exp (i + 1) s
                Assign(s.[i + 1], read_exp (i + 2) s)          
      | "if" -> let k = find (i + 1) 0 s
                if k <> 0 then
                  let k2 =
                    if s.[k] = "while" then 
                      find (k + 1) 1 s
                    elif s.[k] = "if" || s.[k] = ";" then find (k + 1) 2 s
                    else find (k + 1) 0 s
                  if k2 <> 0 then 
                    If(read_exp (i + 1) s, read k s, read k2 s)
                  else If(read_exp (i + 1) s, read k s, Nil)
                else If(read_exp (i + 1) s, Nil, Nil)
      | "while" -> let k = find (i + 1) 0 s
                   if k <> 0 then
                     While(read_exp (i + 1) s, read k s)
                   else While(read_exp (i + 1) s, Nil)    
      | "read" -> let k = read_exp (i + 1) s
                  Read s.[i + 1]
      | "write" -> Write(read_exp (i + 1) s) 
      | _   -> read (i + 1) s
    else Nil

let inter (s : string [], values_list : int [],  where_print : bool) =
  let myTree = read 0 s
  let var_array = !listOfVar // make an array of variables

  let program (value_array : int [], var_array : string []) t = // execute all commands
    let mutable val_a = Array.create var_array.Length None
    let size = ref -1
    use stream = new StreamWriter("output.txt")

    let rec action tree val_ar =
      match tree with
      | Node(t1, t2)    -> action t1 val_ar; action t2 val_ar 
      | If(exp, t1, t2) -> if (calc exp val_ar var_array > 0) then action t1 val_ar
                           else action t2 val_ar
      | While(exp, t)   -> while (calc exp val_ar var_array > 0) do
                             action t val_ar
      | Read str        -> size := !size + 1
                           let k = find_var 0 str var_array
                           if !size >= value_array.Length then 
                             printfn "Enter %A" str
                             val_ar.[k] <- Some (int (System.Console.ReadLine()))
                           else val_ar.[k] <- Some value_array.[!size];              
      | Write exp       -> if where_print then stream.WriteLine(calc exp val_ar var_array)
                           else printfn "%A" (calc exp val_ar var_array)
      | Assign(s, exp)  -> let k = find_var 0 s var_array
                           val_ar.[k] <- Some (calc exp val_ar var_array)
      | _               -> () 
    action t val_a
  program (values_list, var_array) myTree

