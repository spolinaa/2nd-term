(* Calculator with GUI
(expectation: 10 h; reality: 15 h)
by Sokolova Polina *)

module Calc

open System.Windows.Forms
open System.Drawing

exception DivByZero
exception NegUnderRoot
exception DataOverflow

type Exp = 
  | Nil
  | Num of string
  | Op of Exp * string * Exp 

let calcOutput =
  let lbl = new Label()
  lbl.BackColor <- Color.LightSteelBlue
  lbl.Location  <- System.Drawing.Point(40, 30)
  lbl.Font      <- new Font("Courier", 24.0f);
  lbl.Visible   <- true
  lbl.Height    <- 65
  lbl.Width     <- 325
  lbl


let floatMake (a : Exp) =
  match a with
  | Num x -> float x
  | _     -> 0.0

let operation (res : float) 
  =
  if (res % 1.0 = 0.0) then 
    calcOutput.Text <- sprintf "%A" (int res)
    Num (string (int res))
  else 
    calcOutput.Text <- sprintf "%A" (res)
    Num (string res)



let rec calc exp =
  match exp with
  | Nil   -> calcOutput.Text <- sprintf "0"; Nil 
  | Num x -> calcOutput.Text <- sprintf "%A" x; exp
  | Op(a, "√", c) -> let fa = floatMake a
                     if fa >= 0.0 then operation (sqrt fa)
                     else raise (NegUnderRoot)
  | Op(a, b, c)   -> let fa = floatMake a
                     let fc = floatMake c
                     match b with
                     | "+" -> operation (fa + fc)
                     | "-" -> operation (fa - fc)
                     | "*" -> if a <> Nil then 
                                if c <> Nil then
                                  operation (fa * fc)
                                else operation (fa * fa)
                              else calcOutput.Text <- sprintf "0"; Nil
                     | "/" -> if a <> Nil && c <> Nil then  
                                if fc <> 0.0 then operation (fa / fc)
                                else raise (DivByZero)
                              else calcOutput.Text <- sprintf "0"; Nil
                     | _   -> Nil
