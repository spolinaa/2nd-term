(* Interpreter of L
(expectation: 10 h; reality: 21 h)
by Sokolova Polina *)

module Program

open System.IO

let read (name : string) =
  use stream = new StreamReader(name)
  stream.ReadToEnd()

[<EntryPoint>]
let main args =
  0

