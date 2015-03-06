 (* Some operations (+, -, *, power) with Peano numbers
  (expectation: 2 h; reality: 4 h)
  by Sokolova Polina *)

type Peano = Zero | S of Peano

let suc (p : Peano) = S p
let minus1 (p : Peano) =
    match p with
    | Zero -> Zero
    | S  p -> p

let rec plus a b =
    match a with
    | Zero -> b
    | S  a -> S (plus a b)

let rec minus a b =
    match (a, b) with
    | Zero, _  -> Zero
    | a, Zero  -> a
    | S a, S b -> minus a b

let rec multi a b =
    match (a, b) with
    | Zero, _ -> Zero
    | _, Zero -> Zero
    | a, S  b -> plus (multi a b) a

let rec PeanoToString a =
    match a with
    | Zero -> 0
    | S  a -> (PeanoToString a) + 1

let rec power b a  =
    match (a, b) with
    | Zero, _ -> Zero
    | _, Zero -> S Zero
    | a, S  b -> multi (power b a) a

[<EntryPoint>]
let main args =
    let a = S ( S ( S ( S ( S Zero ))))
    let b = S ( S ( S Zero ))
    printf "%A\n" (PeanoToString (plus a b))
    printf "%A\n" (PeanoToString (minus a b))
    printf "%A\n" (PeanoToString (multi a b))
    printf "%A\n" (PeanoToString (power b a))
    0