(* Interfaces of Graph
(expectation: 5 h; reality: 4,5 h)
by Sokolova Polina *)

type IList<'A> = 
    abstract InsertB     : 'A -> unit
    abstract InsertE     : 'A -> unit
    abstract InsertByNum : 'A -> int -> unit
    abstract DeleteB     : unit
    abstract DeleteE     : unit
    abstract DeleteByNum : int -> unit
    abstract Find        : ('A -> bool) -> Option<'A>
    abstract ReturnList  : 'A list
    abstract ReturnArray : 'A []
    abstract Concat      : IList<'A> -> unit
    abstract Printf      : unit

type ADTList<'A> (list : 'A list) =
    class
        let mutable l = list

        interface IList<'A> with 
            member s.InsertB a =
                l <- a :: l
            member s.InsertE a = 
                let rec insert e =
                    match e with
                    | []        -> a :: []
                    | h :: list -> h :: (insert list)
                l <- insert l
            member s.InsertByNum var num =
                let rec insert e count : 'A list =
                    match e with
                    | []        -> failwith "Not enough data!"
                    | h :: list -> match compare count num with
                                   | x when x < 0 -> h :: (insert list (count + 1))
                                   | x when x = 0 -> var :: (h :: list)
                                   | _            -> failwith "Wrong argument!"
                l <- insert l 1 
            member s.DeleteB =
                match l with
                | []        -> failwith "An empty list!"
                | h :: list -> l <- list   
            member s.DeleteE =
                let rec delete e =
                    match e with
                    | [] -> failwith "An empty list!"
                    | h :: []   -> []
                    | h :: list -> h :: (delete list)
                l <- delete l
            member s.DeleteByNum num =
                let rec delete e count : 'A list =
                    match e with
                    | []        -> failwith "Not enough data!"
                    | h :: list -> match compare count num with
                                   | x when x < 0 -> h :: (delete list (count + 1))
                                   | x when x = 0 -> list
                                   | _            -> failwith "Wrong argument!"
                l <- delete l 1
            member s.Find p =
                let rec find e =
                    match e with
                    | []        -> None
                    | h :: list -> if (p h) then Some h
                                   else find list
                find l
            member s.ReturnList = l
            member s.ReturnArray = List.toArray l
            member s.Concat list =
                let rec append left right =
                    match left with
                    | []        -> right
                    | h :: t -> h :: (append t right)
                l <- append l list.ReturnList
            member s.Printf =
                printfn "%A" l
    end

type ArrayList<'A> (array : 'A []) =
    class
        let mutable arr = array 
        member s.ReturnArray = arr

        interface IList<'A> with 
            member s.InsertB a =
                arr <- Array.append [|a|] arr
            member s.InsertE a = 
                arr <- Array.append arr [|a|]
            member s.InsertByNum var num =
                arr <- Array.append (Array.append arr.[0..(num - 1)] [|var|]) arr.[num..(arr.Length - 1)]
            member s.DeleteB =
                arr <- Array.append arr.[1..(arr.Length - 1)] [||]  
            member s.DeleteE =
                arr <- Array.append arr.[0..(arr.Length - 2)] [||]
            member s.DeleteByNum num =
                arr <- Array.append arr.[0..(num - 1)] arr.[(num + 1)..(arr.Length - 1)]
            member s.Find p =
                Array.tryFind p arr
            member s.ReturnList = Array.toList arr
            member s.ReturnArray = arr 
            member s.Concat a2 =
                arr <- Array.append arr a2.ReturnArray
            member s.Printf =
                printfn "%A" arr
    end

[<EntryPoint>]
let main args =
    let list1 = [1; 5; 0; 7; 2; 5; 3; 8; 9]
    let list2 = [2; 5; 6]
    let MyList1 = new ADTList<int> (list1)
    let MyList2 = new ADTList<int> (list2)
    printf "MyList1                  : " 
    (MyList1 :> IList<int>).Printf
    printf "Add 18 to the beginning  : "
    (MyList1 :> IList<int>).InsertB 18
    (MyList1 :> IList<int>).Printf
    printf "Add 256 to the end       : "
    (MyList1 :> IList<int>).InsertE 256
    (MyList1 :> IList<int>).Printf
    printf "Add 1 to the 7th place   : "
    (MyList1 :> IList<int>).InsertByNum 1 7
    (MyList1 :> IList<int>).Printf
    printf "Delete from the beginning: "
    (MyList1 :> IList<int>).DeleteB 
    (MyList1 :> IList<int>).Printf
    printf "Delete from the end      : "
    (MyList1 :> IList<int>).DeleteE 
    (MyList1 :> IList<int>).Printf
    printf "Delete from the 6th place: "
    (MyList1 :> IList<int>).DeleteByNum 6
    (MyList1 :> IList<int>).Printf
    printf "MyList2                  : " 
    (MyList2 :> IList<int>).Printf
    printf "Concat MyList1 & MyList2 : "
    (MyList1 :> IList<int>).Concat MyList2
    (MyList1 :> IList<int>).Printf

    let array1 = [|'n'; 'e'; 'v'; 'e'; 'r'; 'g'; 'a'; 'v'; 'e'; 'u'; 'p'|] 
    let array2 = [|','; 'd'; 'u'; 'd'; 'e'; '!'|]
    let MyArray1 = new ArrayList<char> (array1)
    let MyArray2 = new ArrayList<char> (array2)
    printf "MyArray1                  : " 
    (MyArray1 :> IList<char>).Printf
    printf "Add 'I' to the beginning  : "
    (MyArray1 :> IList<char>).InsertB 'I'
    (MyArray1 :> IList<char>).Printf
    printf "Add '!' to the end        : "
    (MyArray1 :> IList<char>).InsertE '!'
    (MyArray1 :> IList<char>).Printf
    printf "Add 'a' to the 8th place  : " // нумерация с 0
    (MyArray1 :> IList<char>).InsertByNum 'i' 8
    (MyArray1 :> IList<char>).Printf
    printf "Delete from the 7th place : "
    (MyArray1 :> IList<char>).DeleteByNum 7
    (MyArray1 :> IList<char>).Printf
    printf "Delete from the beginning : "
    (MyArray1 :> IList<char>).DeleteB 
    (MyArray1 :> IList<char>).Printf
    printf "Delete from the end       : "
    (MyArray1 :> IList<char>).DeleteE 
    (MyArray1 :> IList<char>).Printf
    printf "MyArray2                  : " 
    (MyArray2 :> IList<char>).Printf
    printf "Concat MyArray1 & MyArray2: "
    (MyArray1 :> IList<char>).Concat MyArray2
    (MyArray1 :> IList<char>).Printf
    0