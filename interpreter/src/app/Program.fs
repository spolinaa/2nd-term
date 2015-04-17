module Program

open System.IO
open Interpreter
open Parser

let read (name : string) =
  use stream = new StreamReader(name)
  stream.ReadToEnd()

[<EntryPoint>]
let main args =
//let s = "write\nx"
  let s = ";\n:=\nx\n5\n;\n:=\nx\n-\nx\n1\nwrite\nx"
  printfn "%A" s
  inter ((parse s), [||], false)
  0

