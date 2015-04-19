(* Interpreter of L
(expectation: 10 h; reality: 22 h)
by Sokolova Polina *)

module Program

open System.IO
open Interpreter
open Parser

let read (name : string) =
  use stream = new StreamReader(name)
  stream.ReadToEnd()

let real f l a =
  inter (parse (read(f)), l, a) 

[<EntryPoint>]
let main args =
  0

