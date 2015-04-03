(* Calculator for positive numbers
(expectation: 3 h; reality: 39 h)
by Sokolova Polina *)

module calculator
open NUnit.Framework

type Tree<'A> = Nil | Node of Tree<string> * string * Tree<string>

let lists (s : string) =
  let length = s.Length
  let rec make t i (n : string) (child : Tree<string>) : Tree<string> =
    let mutable num = n
    if i < length then
      match s.[i] with
      | ' ' -> make t (i + 1) num child
      | '0'| '1'| '2'| '3'| '4'| '5'| '6'| '7'| '8'| '9' -> make t (i + 1) (num + s.[i].ToString()) child
      | '^'| '*'| '/'| '%'| '+'| '-' -> let mutable tree = t   
                                        if num = null then
                                          match t with
                                          | Nil -> tree <- Node(child, s.[i].ToString(), (make Nil (i + 1) null Nil))
                                          | _ -> ()
                                        else
                                          match t with
                                          | Nil -> tree <- Node(Node(Nil, num, Nil), s.[i].ToString(), (make Nil (i + 1) null Nil))
                                          | _ -> ()

                                        let rec myfind i count fu =
                                          if i < length && count > - 1 then
                                            match s.[i] with
                                            | '(' -> myfind (i + 1) (count + 1) fu
                                            | ')' -> myfind (i + 1) (count - 1) fu
                                            | '+'| '-' | '*'| '/'| '%'| '^'   -> if count = 0 && fu i then i
                                                                                 else myfind (i + 1) count fu
                                            | _ -> myfind (i + 1) count fu
                                          else -1

                                        let plus_minus i =
                                          match s.[i] with
                                          | '+'| '-' -> true
                                          | _ -> false

                                        let all i =
                                          match s.[i] with
                                          | '+'| '-'| '*'| '/'| '%' -> true
                                          | _ -> false

                                        let rebuild t =
                                          match t with
                                            | Node(L, C, Node(l, c, r)) -> Node(Node(L, C, l), c, r)
                                            | _ -> t

                                        if s.[i] = '+' || s.[i] = '-' then 
                                          if (myfind (i + 1) 0 plus_minus) > 0 then tree <- rebuild tree
                                        elif (myfind (i + 1) 0 all) > 0 then tree <- rebuild tree
                                        tree 
      | '(' ->  let mutable tree = make Nil (i + 1) null Nil
                let rec f i =
                  if i < length then
                    match s.[i] with
                    | '+'| '-'| '*'| '/'| '%'| '^' -> i 
                    | ')' -> 0
                    | '(' -> failwith "Incorrect enter"
                    | _   -> f (i + 1)
                  else 0

                let rec find i count =
                  if i < length && count > - 1 then
                    match s.[i] with
                    | '(' -> find (i + 1) (count + 1)
                    | ')' -> if count = 0 then f (i + 1)
                             else find (i + 1) (count - 1)
                    | _   -> find (i + 1) count
                  else -1

                let k = find (i + 1) 0
                if k > 0 then
                  make Nil k null tree
                else tree 
      | ')' -> let x = num
               num <- null
               Node(Nil, x, Nil)      
      | _ -> failwith "Incorrect symbol"
    elif (num <> null) then Node(Nil, num, Nil)
    else Nil

  let mutable bte = make Nil 0 null Nil
  bte

let rec calc t =
    match t with
    | Nil               -> 0 
    | Node(Nil, c, Nil) -> int c
    | Node(L, c, R)     -> let result =
                             match c with
                             | "+" -> calc L + calc R
                             | "-" -> calc L - calc R
                             | "*" -> calc L * calc R
                             | "/" -> calc L / calc R
                             | "%" -> calc L % calc R
                             | "^" -> let mutable res = 1
                                      for i = 1 to calc R do
                                        res <- calc L * res
                                      res
                             | _   -> 0

                           result

[<TestCase("4 * (3 + 2) - 7", Result = 13)>]
[<TestCase("(3 - 1) ^ (4 + 1) - 5", Result = 27)>]
[<TestCase("2^2", Result = 4)>]
[<TestCase("(1 + 2)^(3 + 1) * (5 + 2)", Result = 567)>]
[<TestCase("4 * 3 - 7", Result = 5)>]
[<TestCase("1 + 2^2", Result = 5)>]
[<TestCase("77 - 2 * (6 - 1) + 14", Result = 81)>]
[<TestCase("(5 * 4) ^ (5 - 3)", Result = 400)>]
[<TestCase("(2 + 3)^6 - (5 % 7)^(7 - 1)", Result = 0)>]
[<TestCase("44 + (2 % 7)^(7 - 1) * 16", Result = 1068)>]
[<TestCase("7 / ((5 - 7) * 8) - 4", Result = -4)>]
[<TestCase("50 - (5 * 4) * (5 - 3)", Result = 10)>]
[<TestCase("25 + (7 / 3) % (2 * 2) ^ 3 - 1", Result = 26)>]
[<TestCase("4 * 7 / 5", Result = 5)>]
[<TestCase("2 * 16 + (14 - 2 * (5 + 13 * (6 - 1) ^ (3 - 2)) + 33)", Result = -61)>]
let ``Calculator tests`` (expression) =
  calc (lists expression)

[<EntryPoint>]
let main args =
  0 