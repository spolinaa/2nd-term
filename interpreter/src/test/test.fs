module Test

open NUnit.Framework
open Parser
open Interpreter 
open System.IO

[<TestCase(";\nread\nx\n;\nread\nn\n;\n:=\nres\n1\n;\nwhile\nn\n;\n:=\nres\n*\nres\nx\n:=\nn\n-\nn\n1\nwrite\nres\n", [|7; 2|], Result = "49\n")>]
[<TestCase(";\nread\nx\n;\nread\ny\n;\nif\nx\n:=\nx\n10\nif\ny\n:=\ny\n7\n:=\nx\n5\n;\nwrite\nx\nwrite\ny\n", [|0; 14|], Result = "0\n7\n")>] //вложенные if
[<TestCase(";\nread\nx\nwhile\nx\n;\nread\ny\n;\nwhile\ny\n;\n:=\ny\n-\ny\nx\nwrite\ny\n:=\nx\n-\nx\n1\n", [|3; 4; 5; 2|], Result = "1\n-2\n3\n1\n-1\n1\n0\n")>] //вложенные while
[<TestCase(";\nread\nx\nwhile\nx\n;\nread\ny\n\nif\ny\n;\n:=\ny\n*\ny\nx\n;\nwrite\ny\n:=\nx\n-\nx\n1\n:=\nx\n0\n", [|4; 7; 0|], Result = "28\n")>]
[<TestCase(";\n:=\nx\n5\n;\n:=\nx\n-\nx\n1\nwrite\nx\n", [|5|], Result = "4\n")>]
[<TestCase("write\nx\n", [||], Result = "invalidOp")>]
  let ``Tests for Interpreter`` (s : string, l : int []) =
    inter ((parse s), l, true)
    read("output.txt")
