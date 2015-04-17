(* Tests for tasks 28 - 29
(expectation: 3 h; reality: 4,5 h)
by Sokolova Polina *)

module t3
open System
open NUnit.Framework

type IList<'A> = 
  abstract InsertB   : 'A -> unit
  abstract InsertE   : 'A -> unit
  abstract InsertByNum : 'A -> int -> bool
  abstract DeleteB   : bool
  abstract DeleteE   : bool
  abstract DeleteByNum : int -> bool
  abstract Find  : ('A -> bool) -> Option<'A>
  abstract ReturnList  : 'A list
  abstract ReturnArray : 'A []
  abstract Concat  : IList<'A> -> unit
  abstract Printf  : unit

type ADTList<'A when 'A : equality> (list : 'A list) =
  class
    let mutable l = list

    interface IList<'A> with 
      member s.InsertB a =
        l <- a :: l
      member s.InsertE a = 
        let rec insert e =
          match e with
          | []  -> a :: []
          | h :: list -> h :: (insert list)
        l <- insert l
      member s.InsertByNum var num =
        if (num < ((List.length l) + 2)) && (num > 0) then
          let rec insert e count : 'A list =
            match e with
            | []  -> var :: []
            | h :: list -> if (count - 1) = 0 then var :: (h :: list)
                           else h :: (insert list (count - 2))
          l <- insert l num; true
        else false
      member s.DeleteB =
        match l with
        | []  -> false
        | h :: list -> l <- list; true   
      member s.DeleteE =
        let rec delete e =
          match e with
          | []  -> []
          | h :: []   -> []
          | h :: list -> h :: (delete list)
        if (l = []) then false
        else l <- delete l; true
      member s.DeleteByNum num =
        let rec delete e count : 'A list =
          match e with
          | []  -> []
          | h :: list -> if (count - 1) = 0 then list
                         else h :: (delete list (count - 2))
        if l.Length <= num then false
        else l <- delete l num; true
      member s.Find p =
        let rec find e =
          match e with
          | []  -> None
          | h :: list -> if (p h) then Some h
                         else find list
        find l
      member s.ReturnList = l
      member s.ReturnArray = List.toArray l
      member s.Concat list =
        let rec append left right =
          match left with
          | []  -> right
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
        if (num > arr.Length) || (num < 0) then false
        else arr <- Array.append (Array.append arr.[0..(num - 1)] [|var|]) 
                    arr.[num..(arr.Length - 1)]; true
      member s.DeleteB =
        if (arr.Length = 0) then false
        else arr <- Array.append arr.[1..(arr.Length - 1)] [||]; true
      member s.DeleteE =
        if (arr.Length = 0) then false 
        else arr <- Array.append arr.[0..(arr.Length - 2)] [||]; true
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

[<TestFixture>]
type ``Getting an empty list`` () = 
  let adt = new ADTList<int> ([]) :> IList<int>

  [<Test>]
  member x.``Add 1 to the beginning``() =
    let adt = new ADTList<int> ([]) :> IList<int>
    adt.InsertB 1
    Assert.AreEqual(adt.ReturnList, [1])

  [<Test>]
  member x.``Add 10 to the end``() =
    let adt = new ADTList<int> ([]) :> IList<int>
    adt.InsertE 10
    Assert.AreEqual(adt.ReturnList, [10])

  [<TestCase (3, Result = false)>]
  [<TestCase (0, Result = false)>]
  member x.``Add 5``(place) =
    adt.InsertByNum 5 place

  [<Test>]
  member x.``Delete from the beginning``() =
    Assert.AreEqual(adt.DeleteB, false)

  [<Test>]
  member x.``Delete from the end``() =
    Assert.AreEqual(adt.DeleteE, false)

  [<Test>]
  member x.``Delete a number from the 3rd place``() =
    Assert.AreEqual(adt.DeleteByNum 3, false)

  [<Test>]
  member x.``Find a number bigger than 4``() =
    Assert.AreEqual(adt.Find ((<) 4), None)

  [<Test>]
  member x.``Return an empty list``() =
    Assert.AreEqual(adt.ReturnList, [])

  [<Test>]
  member x.``Return an empty array``() =
    Assert.AreEqual(adt.ReturnArray, [||])
  
  [<Test>]
  member x.``Concat with an empty list``() =
    let adt = new ADTList<int> ([]) :> IList<int>
    let adt2 = new ADTList<int> ([]) :> IList<int>
    adt.Concat adt2
    Assert.AreEqual(adt.ReturnList, [])

  [<Test>]
  member x.``Concat with 1; 2``() =
    let adt = new ADTList<int> ([]) :> IList<int>
    let adt2 = new ADTList<int> ([1; 2]) :> IList<int>
    adt.Concat adt2
    Assert.AreEqual(adt.ReturnList, [1; 2])

type ``Getting a list of 1; 2; 3; 4; 5`` () = 

  [<Test>]
  member x.``Add 1 to the beginning``() =
    let adt = new ADTList<int> ([1; 2; 3; 4; 5]) :> IList<int>
    adt.InsertB 0
    Assert.AreEqual(adt.ReturnList, [0; 1; 2; 3; 4; 5])

  [<Test>]
  member x.``Add 6 to the end``() =
    let adt = new ADTList<int> ([1; 2; 3; 4; 5]) :> IList<int>
    adt.InsertE 6
    Assert.AreEqual(adt.ReturnList, [1; 2; 3; 4; 5; 6])

  [<TestCase(7, 2, Result = true)>]
  [<TestCase(7, 7, Result = false)>]
  [<TestCase(6, 6, Result = true)>]
  member x.``Add a number on place``(number, place) =
    let adt = new ADTList<int> ([1; 2; 3; 4; 5]) :> IList<int>
    adt.InsertByNum number place

  [<Test>]
  member x.``Delete from the beginning``() =
    let adt = new ADTList<int> ([1; 2; 3; 4; 5]) :> IList<int>
    Assert.AreEqual(adt.DeleteB, true)

  [<Test>]
  member x.``Delete from the end``() =
    let adt = new ADTList<int> ([1; 2; 3; 4; 5]) :> IList<int>
    Assert.AreEqual(adt.DeleteE, true)

  [<TestCase(1, Result = true)>]
  [<TestCase(6, Result = false)>]
  member x.``Delete a number from a place``(number) =
    let adt = new ADTList<int> ([1; 2; 3; 4; 5]) :> IList<int>
    adt.DeleteByNum number

  [<Test>]
  member x.``Find a number bigger than 10``() =
    let adt = new ADTList<int> ([1; 2; 3; 4; 5]) :> IList<int>
    Assert.AreEqual(adt.Find ((<) 10), None)

  [<Test>]
  member x.``Find a number less than 3``() =
    let adt = new ADTList<int> ([1; 2; 3; 4; 5]) :> IList<int>
    Assert.AreEqual(adt.Find ((>) 3), Some 1)
  
  [<Test>]
  member x.``Return a list``() =
    let adt = new ADTList<int> ([1; 2; 3; 4; 5]) :> IList<int>
    Assert.AreEqual(adt.ReturnList, [1; 2; 3; 4; 5])

  [<Test>]
  member x.``Return an array``() =
    let adt = new ADTList<int> ([1; 2; 3; 4; 5]) :> IList<int>
    Assert.AreEqual(adt.ReturnArray, [|1; 2; 3; 4; 5|])

  [<Test>]
  member x.``Concat with an empty list``() =
    let adt = new ADTList<int> ([1; 2; 3; 4; 5]) :> IList<int>
    let adt2 = new ADTList<int> ([]) :> IList<int>
    adt.Concat adt2
    Assert.AreEqual(adt.ReturnList, [1; 2; 3; 4; 5])

  [<Test>]
  member x.``Concat with 6; 7``() =
    let adt = new ADTList<int> ([1]) :> IList<int>
    let adt2 = new ADTList<int> ([6; 7]) :> IList<int>
    adt.Concat adt2
    Assert.AreEqual(adt.ReturnList, [1; 6; 7])


[<TestFixture>]
type ``Getting an empty array`` () = 
  let array = new ArrayList<int> ([||]) :> IList<int>

  [<Test>]
  member x.``Add 1 to the beginning``() =
    let array = new ArrayList<int> ([||]) :> IList<int>
    array.InsertB 1
    Assert.AreEqual(array.ReturnList, [1])

  [<Test>]
  member x.``Add 10 to the end``() =
    let array = new ArrayList<int> ([||]) :> IList<int>
    array.InsertE 10
    Assert.AreEqual(array.ReturnList, [10])

  [<TestCase(5, 0, Result = true)>]
  [<TestCase(5, 3, Result = false)>]
  member x.``Add a number``(number, place) =
    let array = new ArrayList<int> ([||]) :> IList<int>
    array.InsertByNum number place

  [<Test>]
  member x.``Delete from the beginning``() =
    Assert.AreEqual(array.DeleteB, false)

  [<Test>]
  member x.``Delete from the end``() =
    Assert.AreEqual(array.DeleteE, false)

  [<Test>]
  member x.``Delete a number from the 3rd place``() =
    Assert.AreEqual(array.DeleteByNum 3, false)

  [<Test>]
  member x.``Find a number bigger than 4``() =
    Assert.AreEqual(array.Find ((<) 4), None)

  [<Test>]
  member x.``Return an empty list``() =
    Assert.AreEqual(array.ReturnList, [])

  [<Test>]
  member x.``Return an empty array``() =
    Assert.AreEqual(array.ReturnArray, [||])

  [<Test>]
  member x.``Concat with an empty list``() =
    let array = new ArrayList<int> ([||]) :> IList<int>
    let array2 = new ArrayList<int> ([||]) :> IList<int>
    array.Concat array2
    Assert.AreEqual(array.ReturnList, [])

  [<Test>]
  member x.``Concat with 1; 2``() =
    let array = new ArrayList<int> ([||]) :> IList<int>
    let array2 = new ArrayList<int> ([|1; 2|]) :> IList<int>
    array.Concat array2
    Assert.AreEqual(array.ReturnList, [1; 2])

type ``Getting an array of 1; 2; 3; 4; 5`` () = 

  [<Test>]
  member x.``Add 1 to the beginning``() =
    let array = new ArrayList<int> ([|1; 2; 3; 4; 5|]) :> IList<int>
    array.InsertB 0
    Assert.AreEqual(array.ReturnList, [0; 1; 2; 3; 4; 5])

  [<Test>]
  member x.``Add 6 to the end``() =
    let array = new ArrayList<int> ([|1; 2; 3; 4; 5|]) :> IList<int>
    array.InsertE 6
    Assert.AreEqual(array.ReturnList, [1; 2; 3; 4; 5; 6])

  [<TestCase(7, 1, Result = true)>]
  [<TestCase(6, -1, Result = false)>]
  [<TestCase(7, 6, Result = false)>]
  member x.``Add a number``(number, place) =
    let array = new ArrayList<int> ([|1; 2; 3; 4; 5|]) :> IList<int>
    array.InsertByNum number place

  [<Test>]
  member x.``Delete from the beginning``() =
    let array = new ArrayList<int> ([|1; 2; 3; 4; 5|]) :> IList<int>
    Assert.AreEqual(array.DeleteB, true)

  [<Test>]
  member x.``Delete from the end``() =
    let array = new ArrayList<int> ([|1; 2; 3; 4; 5|]) :> IList<int>
    Assert.AreEqual(array.DeleteE, true)

  [<TestCase(0, Result = true)>]
  [<TestCase(5, Result = false)>]
  member x.``Delete a number from the place``(place) =
    let array = new ArrayList<int> ([|1; 2; 3; 4; 5|]) :> IList<int>
    array.DeleteByNum place

  [<Test>]
  member x.``Find a number bigger than 10``() =
    let array = new ArrayList<int> ([|1; 2; 3; 4; 5|]) :> IList<int>
    Assert.AreEqual(array.Find ((<) 10), None)

  [<Test>]
  member x.``Find a number less than 3``() =
    let array = new ArrayList<int> ([|1; 2; 3; 4; 5|]) :> IList<int>
    Assert.AreEqual(array.Find ((>) 3), Some 1)
  
  [<Test>]
  member x.``Return an list``() =
    let array = new ArrayList<int> ([|1; 2; 3; 4; 5|]) :> IList<int>
    Assert.AreEqual(array.ReturnList, [1; 2; 3; 4; 5])

  [<Test>]
  member x.``Return an array``() =
    let array = new ArrayList<int> ([|1; 2; 3; 4; 5|]) :> IList<int>
    Assert.AreEqual(array.ReturnArray, [|1; 2; 3; 4; 5|])

  [<Test>]
  member x.``Concat with an empty list``() =
    let array = new ArrayList<int> ([|1; 2; 3; 4; 5|]) :> IList<int>
    let array2 = new ArrayList<int> ([||]) :> IList<int>
    array.Concat array2
    Assert.AreEqual(array.ReturnList, [1; 2; 3; 4; 5])

  [<Test>]
  member x.``Concat with 6; 7``() =
    let array = new ArrayList<int> ([|1|]) :> IList<int>
    let array2 = new ArrayList<int> ([|6; 7|]) :> IList<int>
    array.Concat array2
    Assert.AreEqual(array.ReturnList, [1; 6; 7])

[<EntryPoint>]
let main argv = 
  0

     