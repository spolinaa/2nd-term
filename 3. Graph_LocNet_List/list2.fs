(* Interfaces of PolyList
(expectation: 5 h; reality: 4,5 h)
by Sokolova Polina *)

type IList<'A> = 
    abstract InsertB     : 'A -> unit
    abstract InsertE     : 'A -> unit
    abstract InsertByNum : 'A -> int -> bool
    abstract DeleteB     : bool
    abstract DeleteE     : bool
    abstract DeleteByNum : int -> bool
    abstract Find        : ('A -> bool) -> Option<'A>
    abstract ReturnList  : 'A list
    abstract ReturnArray : 'A []
    abstract Concat      : IList<'A> -> unit
    abstract Printf      : unit

type ADTList<'A when 'A : equality> (list : 'A list) =
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
                    | []        -> []
                    | h :: list -> if count = 0 then var :: (h :: list)
                                   else h :: (insert list (count - 1))
                l <- insert l num
                if (l = []) then false
                else true
            member s.DeleteB =
                match l with
                | []        -> false
                | h :: list -> l <- list; true   
            member s.DeleteE =
                let rec delete e =
                    match e with
                    | []        -> []
                    | h :: []   -> []
                    | h :: list -> h :: (delete list)
                if (l = []) then false
                else l <- delete l; true
            member s.DeleteByNum num =
                let rec delete e count : 'A list =
                    match e with
                    | []        -> []
                    | h :: list -> if count = 0 then list
                                   else h :: (delete list (count - 1))
                if l.Length <= num then false
                else l <- delete l num; true
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

        interface IList<'A> with 
            member s.InsertB a =
                arr <- Array.append [|a|] arr
            member s.InsertE a = 
                arr <- Array.append arr [|a|]
            member s.InsertByNum var num =
                if num >= arr.Length then false
                else arr <- Array.append (Array.append arr.[0..(num - 1)] [|var|]) 
                                          arr.[num..(arr.Length - 1)]; true
            member s.DeleteB =
                arr <- Array.append arr.[1..(arr.Length - 1)] [||]
                true
            member s.DeleteE =
                arr <- Array.append arr.[0..(arr.Length - 2)] [||]
                true
            member s.DeleteByNum num =
                if num >= arr.Length then false
                else arr <- Array.append arr.[0..(num - 1)] 
                            arr.[(num + 1)..(arr.Length - 1)]; true
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
    let MyList1 = new ADTList<int> (list1) :> IList<int>
    let MyList2 = new ADTList<int> (list2) :> IList<int>
    printf "MyList1                  : " 
    MyList1.Printf
    printf "Add 18 to the beginning  : "
    MyList1.InsertB 18
    MyList1.Printf
    printf "Add 256 to the end       : "
    MyList1.InsertE 256
    MyList1.Printf
    printf "Add 1 to the 7th place   : "
    let a = MyList1.InsertByNum 1 7
    MyList1.Printf
    printf "Delete from the beginning: "
    let a = MyList1.DeleteB 
    MyList1.Printf
    printf "Delete from the end      : "
    let a = MyList1.DeleteE 
    MyList1.Printf
    printf "Delete from the 6th place: "
    let a = MyList1.DeleteByNum 6
    MyList1.Printf
    printf "MyList2                  : " 
    MyList2.Printf
    printf "Concat MyList1 & MyList2 : "
    MyList1.Concat MyList2
    MyList1.Printf

    printfn ""
    let array1 = [|'n'; 'e'; 'v'; 'e'; 'r'; 'g'; 'a'; 'v'; 'e'; 'u'; 'p'|] 
    let array2 = [|','; 'd'; 'u'; 'd'; 'e'; '!'|]
    let MyArray1 = new ArrayList<char> (array1) :> IList<char>
    let MyArray2 = new ArrayList<char> (array2) :> IList<char>
    printf "MyArray1                  : " 
    MyArray1.Printf
    printf "Add 'I' to the beginning  : "
    MyArray1.InsertB 'I'
    MyArray1.Printf
    printf "Add '!' to the end        : "
    MyArray1.InsertE '!'
    MyArray1.Printf
    printf "Add 'a' to the 8th place  : " // нумерация с 0
    let a = MyArray1.InsertByNum 'i' 8
    MyArray1.Printf
    printf "Delete from the 7th place : "
    let a = MyArray1.DeleteByNum 7
    MyArray1.Printf
    printf "Delete from the beginning : "
    let a = MyArray1.DeleteB 
    MyArray1.Printf
    printf "Delete from the end       : "
    let a = MyArray1.DeleteE 
    MyArray1.Printf
    printf "MyArray2                  : " 
    MyArray2.Printf
    printf "Concat MyArray1 & MyArray2: "
    MyArray1.Concat MyArray2
    MyArray1.Printf
    0