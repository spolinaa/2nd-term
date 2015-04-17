(* Stack machine
(expectation: 2 h; reality: 5 h)
by Sokolova Polina *)

module A

open System.IO
open NUnit.Framework

type Stack<'A> = Nil | Cons of 'A * Stack<'A>

type IStack<'A when 'A : equality> () = 
  class
    let mutable s = Nil
    member x.Pop () : 'A = 
      match s with
      | Nil -> failwith "An empty stack!"
      | Cons(a, stack) -> s <- stack; a

    member x.Push a = s <- Cons (a, s)
    member x.IsEmpty = 
      match s with
      | Nil -> true
      | _   -> false
    member x.State = s
    member x.Top = 
      match s with
      | Cons (x, _) -> x
      | _ -> failwith "Error"
  end

let result (a, b : int, c : char) =
    match c with
    | '+' -> a + b
    | '-' -> a - b
    | '*' -> a * b
    | '/' -> a / b
    | '%' -> a % b
    | '^' -> let mutable res = 1
             for i = 1 to b do
               res <- a * res
             res
    | _   -> 0

let stacks (str : string, x : IStack<int>) =
  let mutable num = null
  let mutable list = []
  for i = 0 to (str.Length - 1) do
    match str.[i] with
    | ' '| '\n' -> if num <> null then (x.Push (int num)) 
                   num <- null
    | '0'| '1'| '2'| '3'| '4'| '5'| '6'| '7'| '8'| '9' -> num <- num + str.[i].ToString()
    | '^'| '*'| '/'| '%'| '+'| '-' -> let b = x.Pop()
                                      let a = x.Pop() 
                                      x.Push (result (a, b, str.[i]))

    | _ -> failwith "Incorrect enter!"
  x.Top

let write (name : string, res : 'A) =
  use stream = new StreamWriter(name)
  stream.WriteLine(res)

let read (name : string) =
  use stream = new StreamReader(name)
  stream.ReadToEnd()

[<TestCase("4\n3\n2\n+\n*\n7\n-\n\n", Result = "13\n")>]
[<TestCase("3\n1\n-\n4\n1\n+\n^\n5\n-\n\n", Result = "27\n")>]
[<TestCase("2\n2\n^\n\n", Result = "4\n")>]
[<TestCase("1\n2\n+\n3\n1\n+\n^\n5\n2\n+\n*\n\n", Result = "567\n")>]
[<TestCase("4\n3\n*\n7\n-\n\n", Result = "5\n")>]
[<TestCase("1\n2\n2\n^\n+\n\n", Result = "5\n")>]
[<TestCase("77\n2\n6\n1\n-\n*\n-\n14\n+\n\n", Result = "81\n")>]
[<TestCase("5\n4\n*\n5\n3\n-\n^\n\n", Result = "400\n")>]
[<TestCase("2\n3\n+\n6\n^\n5\n7\n%\n7\n1\n-\n^\n-\n\n", Result = "0\n")>]
[<TestCase("44\n2\n7\n%\n7\n1\n-\n^\n16\n*\n+\n\n", Result = "1068\n")>]
[<TestCase("7\n5\n7\n-\n8\n*\n/\n4\n-\n\n", Result = "-4\n")>]
[<TestCase("50\n5\n4\n*\n5\n3\n-\n*\n-\n\n", Result = "10\n")>]
[<TestCase("25\n7\n3\n/\n2\n2\n*\n3\n^\n%\n+\n1\n-\n\n", Result = "26\n")>]
[<TestCase("4\n7\n*\n5\n/\n\n", Result = "5\n")>]
[<TestCase("2\n16\n*\n14\n2\n5\n13\n6\n1\n-\n3\n2\n-\n^\n*\n+\n*\n-\n33\n+\n+\n\n", Result = "-61\n")>] 
let ``Calculator tests`` (expression) =
  write("input.txt", expression)
  let a = read("input.txt")
  let s1 = new IStack<int>()
  write ("output.txt", stacks (a, s1))
  read("output.txt")

[<EntryPoint>]
let main args =
  write("input.txt", "7\n6\n5\n+\n*")
  let a = read("input.txt")
  let s1 = new IStack<int>()
  write ("output.txt", stacks (a, s1))
  0