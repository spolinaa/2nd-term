// Some operations with Peano numbers
// by Sokolova Polina  

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

let rec PeanoToString a =
    match a with
    | Zero -> 0
    | S  a -> (PeanoToString a) + 1 // "отбрасывает" от a S, пока a не станет Zero

let rec power b a  =
    match (a, b) with
    | (Zero, _) -> Zero
    | (_, Zero) -> S Zero
    | (a, S  b) -> multi (power b a) a // умножает a на саму себя, пока b не станет Zero

[<EntryPoint>]
let main args =
    let a = S ( S ( S ( S ( S Zero ))))
    let b = S ( S ( S Zero ))
    printf "%A\n" ( PeanoToString (plus a b) )
    printf "%A\n" ( PeanoToString (minus a b) )
    printf "%A\n" ( PeanoToString (multi a b) )
    printf "%A\n" ( PeanoToString (power b a) ) 
    0