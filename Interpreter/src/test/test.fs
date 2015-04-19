(* Tests for interpreter of L
by Sokolova Polina *)

module Test

open NUnit.Framework
open Parser
open Interpreter 
open Program

[<TestCase("../../1.txt", [|7; 2|], Result = "49\n")>]
[<TestCase("../../2.txt", [|0; 14|], Result = "0\n7\n")>] //вложенные if
[<TestCase("../../3.txt", [|3; 4; 5; 2|], Result = "1\n-2\n3\n1\n-1\n1\n0\n")>] //вложенные while
[<TestCase("../../4.txt", [|4; 7; 0|], Result = "28\n")>]
[<TestCase("../../5.txt", [|5|], Result = "4\n")>]
  let ``Tests for Interpreter`` (f : string, l : int []) =
    real f l true
    read("output.txt")