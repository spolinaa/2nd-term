type Peano = Zero | S of Peano

let suc (p : Peano) = S p
let minus1 (p : Peano) =
  match p with
  | Zero -> Zero
  | S  p -> p

let rec plus a b = 
  match a with
  | Zero -> b
  | S  a -> S ( plus a b ) // "выкидывает" S из a, доводя до Zero, в конце приписывает b

let rec minus a b =
  match (a, b) with
  | (Zero,  _) -> Zero 
  | (a,  Zero) -> a 
  | (S a, S b) -> minus a b // "доводит" 1 из чисел до Zero, разностью будет "доведенное" значение второго

let rec multi a b =
    match (a, b) with
    | (Zero, _) -> Zero
    | (_, Zero) -> Zero
    | (a, S  b) -> plus (multi a b) a // складывает a с самим собой столько раз, сколько понадобилось для "доведения" b до Zero

let rec PeanoToString a b =
    match a with
    | Zero -> b
    | S  a -> PeanoToString a (b + 1) // "отбрасывает" от a S, пока a не станет Zero, на каждом шаге увеличивает b на 1

let rec power c b a =
    match (a, b) with
    | (Zero, _) -> Zero
    | (_, Zero) -> c
    | (a, S  b) -> power (multi c a) b a

[<EntryPoint>]
let main args =
    let a = Zero
    let b = Zero
    let c = S Zero
    let d = 0
    printf "%A\n" ( PeanoToString (plus a b) d )
    printf "%A\n" ( PeanoToString (minus a b) d )
    printf "%A\n" ( PeanoToString (multi a b) d )
    printf "%A\n" ( PeanoToString (power c b a) d ) 
    0