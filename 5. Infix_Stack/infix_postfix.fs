(* Task 36 (infix -> postfix)
(expectation: 1 h; reality: 1,5 h)
by Sokolova Polina *)

module A

open System.IO
open NUnit.Framework

type Tree<'A> = Nil | Node of Tree<string> * string * Tree<string>

let write (name : string, res : string) =
  use stream = new StreamWriter(name)
  stream.WriteLine(res)

let print t =
  let rec LRCprint tree : string =
    match tree with
    | Nil           -> ""
    | Node(L, c, R) -> LRCprint L + LRCprint R + c.ToString() + "\n" 
  write("output.txt", LRCprint t)

let lists (s : string) =
  let length = s.Length
  let rec make t i (n : string) (child : Tree<string>) : Tree<string> =
    let mutable num = n
    if i < length then
      match s.[i] with
      | ' '| '\n' -> make t (i + 1) num child
      | '0'| '1'| '2'| '3'| '4'| '5'| '6'| '7'| '8'| '9' -> make t (i + 1) (num + s.[i].ToString()) child
      |'+'| '-' | '^'| '*'| '/'| '%' -> let mutable tree = t   
                                        if num = null then
                                          match t with
                                          | Nil -> tree <- Node(child, s.[i].ToString(), (make Nil (i + 1) null Nil))
                                          | _ -> ()
                                        else
                                          match t with
                                          | Nil -> 
                                            tree <- Node(Node(Nil, num, Nil), s.[i].ToString(), (make Nil (i + 1) null Nil))
                                          | _ -> ()

                                        let rec turn_1 tree sign =
                                          if sign > 0 then 
                                            match tree with
                                            | Node(Node(Nil, x, Nil), y, Node(Nil, z, Nil)) -> tree
                                            | Node(L, C, Node(l, c, r)) -> printfn "c = %A" c
                                                                           if c = "+" || c = "-" then
                                                                             Node(turn_1 (Node(L, C, l)) (sign - 1), c, r) 
                                                                           else tree
                                            | _ -> tree
                                          else tree

                                        let rec turn_2 tree sign =
                                          if sign > 0 then
                                            match tree with
                                            | Node(Node(Nil, x, Nil), y, Node(Nil, z, Nil)) -> tree
                                            | Node(L, C, Node(l, c, r)) -> printfn "c = %A" c
                                                                           match c with
                                                                           | "+"| "-"| "*"| "/"| "%" -> 
                                                                             Node(turn_2 (Node(L, C, l)) (sign - 1), c, r)
                                                                           | _ -> tree
                                            | _ -> tree
                                          else tree

                                        let plus_minus i =
                                          match s.[i] with
                                          | '+'| '-' -> true
                                          | _ -> false

                                        let all i =
                                          match s.[i] with
                                          | '+'| '-'| '*'| '/'| '%' -> true
                                          | _ -> false

                                        let rec myfind i count fu sign =
                                          if i < length && count > - 1 then
                                            match s.[i] with
                                            | '(' -> myfind (i + 1) (count + 1) fu sign
                                            | ')' -> myfind (i + 1) (count - 1) fu sign
                                            | '+'| '-' | '*'| '/'| '%'| '^'   -> if count = 0 && fu i then 
                                                                                   myfind (i + 1) count fu (sign + 1)
                                                                                 else myfind (i + 1) count fu sign
                                            | _ -> myfind (i + 1) count fu sign
                                          else sign

                                        printfn "%A" s.[i]
                                        if s.[i] = '+' || s.[i] = '-' then turn_1 tree (myfind (i + 1) 0 plus_minus 0)
                                        else turn_2 tree (myfind (i + 1) 0 all 0)
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
                   
let read (name : string) =
  use stream = new StreamReader(name)
  stream.ReadToEnd()

[<TestCase("4 * (3 + 2) - 7", Result = "4\n3\n2\n+\n*\n7\n-\n\n")>]
[<TestCase("(3 - 1) ^ (4 + 1) - 5", Result = "3\n1\n-\n4\n1\n+\n^\n5\n-\n\n")>]
[<TestCase("2^2", Result = "2\n2\n^\n\n")>]
[<TestCase("(1 + 2)^(3 + 1) * (5 + 2)", Result = "1\n2\n+\n3\n1\n+\n^\n5\n2\n+\n*\n\n")>]
[<TestCase("4 * 3 - 7", Result = "4\n3\n*\n7\n-\n\n")>]
[<TestCase("1 + 2^2", Result = "1\n2\n2\n^\n+\n\n")>]
[<TestCase("77 - 2 * (6 - 1) + 14", Result = "77\n2\n6\n1\n-\n*\n-\n14\n+\n\n")>]
[<TestCase("(5 * 4) ^ (5 - 3)", Result = "5\n4\n*\n5\n3\n-\n^\n\n")>]
[<TestCase("(2 + 3)^6 - (5 % 7)^(7 - 1)", Result = "2\n3\n+\n6\n^\n5\n7\n%\n7\n1\n-\n^\n-\n\n")>]
[<TestCase("44 + (2 % 7)^(7 - 1) * 16", Result = "44\n2\n7\n%\n7\n1\n-\n^\n16\n*\n+\n\n")>]
[<TestCase("7 / ((5 - 7) * 8) - 4", Result = "7\n5\n7\n-\n8\n*\n/\n4\n-\n\n")>]
[<TestCase("50 - (5 * 4) * (5 - 3)", Result = "50\n5\n4\n*\n5\n3\n-\n*\n-\n\n")>]
[<TestCase("25 + (7 / 3) % (2 * 2) ^ 3 - 1", Result = "25\n7\n3\n/\n2\n2\n*\n3\n^\n%\n+\n1\n-\n\n")>]
[<TestCase("4 * 7 / 5", Result = "4\n7\n*\n5\n/\n\n")>]
[<TestCase("2 * 16 + (14 - 2 * (5 + 13 * (6 - 1) ^ (3 - 2)) + 33)", Result = "2\n16\n*\n14\n2\n5\n13\n6\n1\n-\n3\n2\n-\n^\n*\n+\n*\n-\n33\n+\n+\n\n")>] 
[<TestCase("5 + 3 - 2 + 4", Result = "5\n3\n+\n2\n-\n4\n+\n\n")>]
[<TestCase("3^2 - 7 * 14 + 89", Result = "3\n2\n^\n7\n14\n*\n-\n89\n+\n\n")>]
let ``Calculator tests`` (expression) =
  write("input.txt", expression)
  print (lists (read("input.txt")))
  read("output.txt")

[<EntryPoint>]
let main args =
  //write("input.txt", "7 * (6 + 5)")
  print (lists (read("input.txt")))
  0 
