(* Calculator with GUI
(expectation: 10 h; reality: 15 h)
by Sokolova Polina *)

module GUI

open System.Windows.Forms
open System.Drawing
open Calc

let mutable e = Nil

let rec number e num =
  match e with
  | Nil   -> calcOutput.Text <- sprintf "%A" (int num)
             Num num
  | Num x -> 
    calcOutput.Text <- calcOutput.Text + sprintf "%A" (int num)
    if x.ToString().Length < 9 then Num (x.ToString() + num)
    else raise (DataOverflow)
  | Op(a, b, c) -> Op(a, b, number c num) 

let rec dot e num = 
  match e with
  | Nil -> Num "0."
  | Num x -> Num (x + ".")
  | Op(a, b, c) -> Op(a, b, dot c num)

let rec count e =
  match e with
  | Nil           -> calcOutput.Text <- sprintf "0"; Nil
  | Num x         -> e
  | _             -> calc e 

let action (but : Button) e =
  let num = but.Text
  match num with 
  | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"  -> number e num 
  | "+" | "-" | "*" | "/" ->  match e with
                              | Nil   -> Op(Nil, num, Nil)
                              | Num x -> Op(e, num, Nil) 
                              | Op(a, b, Nil) -> Op(a, num, Nil)
                              | Op(a, b, c)   -> Op(calc e, num, Nil) 
  | "->"  ->  let delete =
                let mutable l = calcOutput.Text.Length
                let flag = 
                  if l > 0 then
                    if calcOutput.Text.[0] = '-' then true
                    else false
                  else false
                if flag then l <- l - 1
                match l with
                | x when x > 1 -> 
                  if flag then 
                    calcOutput.Text <- calcOutput.Text.[0..(l - 1)]
                    Num (calcOutput.Text.[0..(l - 1)])
                  else 
                    calcOutput.Text <- calcOutput.Text.[0..(l - 2)]
                    Num (calcOutput.Text.[0..(l - 2)])
                | _            -> calcOutput.Text <- sprintf "0"; Nil
              match e with
              | Nil         -> Nil
              | Num x       -> delete 
              | Op(a, b, c) -> Op(a, b, delete)
  | "+/-" -> match e with
             | Num x -> 
               calcOutput.Text <- sprintf "%A" (-(int x)); Num ((-int x).ToString())
             | _     -> e
  | "√"   -> calcOutput.Text <- sprintf ""
             calc (Op(e, num, Nil)) 
  | "."   -> calcOutput.Text <- calcOutput.Text + sprintf "."
             dot e num
  | "%"   -> calcOutput.Text <- sprintf ""
             match e with
             | Op(Num a, b, Num c) -> 
               let f = float c / 100.0
               if b = "+" || b = "-" then 
                 calc (Op(Num a, b, operation (f * (float a))))
               else calc (Op(Num a, b, operation f)) 
             | Op(Num a, b, Nil) -> 
               if b = "*" then calc (Op(Num a, b, Num ((float a / 100.0).ToString()))) 
               else e
             | _ -> e
  | "="   -> count e 
  | _     -> calcOutput.Text <- sprintf ""; Nil

let calculate but e =
  try action but e with
  | :? NegUnderRoot -> calcOutput.Text <- sprintf "Not defined!"; Nil
  | :? DataOverflow -> calcOutput.Text <- sprintf "Too long number!"; Nil
  | :? DivByZero    -> calcOutput.Text <- sprintf "Division by zero!"; Nil


let numButton i = 
  let array = [|"->" ; "7"; "8"; "9"; "/"; 
                "+/-"; "4"; "5"; "6"; "*"; 
                "√"  ; "1"; "2"; "3"; "-"; 
                "%"  ; "0"; "."; "="; "+"|]
  let button = new Button()
  let size = new Size(45, 45)
  button.BackColor <- Color.LightSkyBlue
  button.Text <- array.[i]
  button.Font <- new Font("Herculanum", 18.0f);
  button.Size <- size
  button.Location <- 
    System.Drawing.Point(70*(i % 5) + 40, 130 + 68*(1 + i / 5))
  button.Click.Add(fun f -> e <- calculate button e)
  button

let offButton =
  let button = new Button()
  let size = new Size(80, 55)
  button.BackColor <- Color.LightBlue
  button.Location  <- System.Drawing.Point(285, 115)
  button.Size <- size
  button.Text <- "OFF"
  button.Font <- new Font("Herculanum", 24.0f);
  button.Click.Add(fun e -> Application.Exit())
  button

let seButton =
  let button = new Button()
  let size = new Size(80, 55)
  button.BackColor <- Color.LightBlue
  button.Location  <- System.Drawing.Point(180, 115)
  button.Size <- size
  button.Text <- "SE"
  button.Font <- new Font("Herculanum", 24.0f);
  button.Click.Add(fun f -> e <- Nil; calcOutput.Text <- sprintf "0")
  button

let mainForm =
  let form = new Form(Visible = false)
  let size = new Size(410, 510)
  form.BackColor <- Color.DeepSkyBlue
  for i = 0 to 19 do
    form.Controls.Add(numButton i)
  form.Controls.Add(offButton)
  form.Controls.Add(seButton)
  form.Controls.Add(calcOutput)
  calcOutput.Text <- sprintf "0"
  form.Size <- size
  form

[<EntryPoint>]
let main args = 
  mainForm.Visible <- true
  Application.Run()
  0