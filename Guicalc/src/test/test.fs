(* Tests for calculator with GUI
by Sokolova Polina *)

module Test

open NUnit.Framework
open System.Windows.Forms
open GUI
open Calc

[<TestCase([|"7"; "8"; "-"; "4"; "="|], Result = 74.0)>]
[<TestCase([|"4"; "1"; "->"|], Result = 4.0)>]
[<TestCase([|"-"; "3"; "2"; "="; "->"|], Result = -3.0)>]
[<TestCase([|"8"; "->"|], Result = 0.0)>]
[<TestCase([|"1"; "0"; "-"; "2"; "0"; "%"|], Result = 8.0)>]
[<TestCase([|"2"; "5"; "√"|], Result = 5.0)>]
[<TestCase([|"9"; "*"; "="|], Result = 81.0)>]
[<TestCase([|"1"; "-"; "1"; "5"; "="; "+/-"|], Result = 14.0)>]
[<TestCase([|"7"; "+/-"|], Result = -7.0)>]
[<TestCase([|"0"; "."; "4"; "1"; "->"|], Result = 0.4)>]
[<TestCase([|"0"; "."; "0"; "7"; "-"; "1"; "."; "2"; "5"; "="; "->"|], Result = -1.1)>]
[<TestCase([|"5"; "/"; "2"; "="|], Result = 2.5)>]
[<TestCase([|"4"; "."; "6"; "7"; "*"; "3"; "."; "2"; "5"; "-"; "0"; "."; "1"; "7"; "7"; "="|], Result = 15.0005)>]
[<TestCase([|"1"; "3"; "+"; "1"; "5"; "="|], Result = 28.0)>]
[<TestCase([|"1"; "7"; "*"; "1"; "2"; "="|], Result = 204.0)>]
[<TestCase([|"1"; "5"; "/"; "1"; "5"; "->"; "="|], Result = 15.0)>]
  let ``Tests for Calculator`` (f : string []) =
    let l = f.Length
    let mutable e = Nil
    for k = 0 to (l - 1) do
      let but = new Button()
      but.Text <- f.[k].ToString()
      e <- calculate but e
    floatMake e