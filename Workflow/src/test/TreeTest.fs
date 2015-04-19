(* Tests for TreeWorkflow
by Sokolova Polina
*)

module TreeTest

open TreeWorkflow
open NUnit.Framework

[<Test>]
let ``Combine Nil Nil`` () =
  Assert.AreEqual(tree.Combine (Nil, Nil), Nil)

[<Test>]
let ``Combine Node(9, Node(1, Nil, Nil)) Node(5, Node(3, Nil, Nil), Node(11, Nil, Nil))`` () =
  let t1 = Node(5, Node(1, Nil, Nil), Nil)
  let t2 = Node(9, Node(7, Nil, Nil), Node(11, Nil, Nil))
  let t3 = Node(9, Node(7, Node(5, Node(1, Nil, Nil), Nil), Nil), Node(11, Nil, Nil))
  Assert.AreEqual(tree.Combine (t1, t2), t3)

[<Test>]
let ``Bind Nil`` () =
  Assert.AreEqual(tree.Bind (Nil, ((+) 11)), Nil)

[<Test>]
let ``Bind Node(7, Node(5, Nil, Nil), Node(9, Nil, Nil))`` () =
  let t1 = Node(7, Node(5, Nil, Nil), Node(9, Nil, Nil))
  let t2 = Node(9, Node(7, Nil, Nil), Node(11, Nil, Nil))
  Assert.AreEqual(tree.Bind (t1, ((+) 2)), t2)

[<Test>]
let ``Map Nil + 16`` () =
  Assert.AreEqual(map ((+) 16) Nil, Nil)

[<Test>]
let ``Map Node(7, Node(5, Nil, Nil)) + 2`` () =
  let t1 = Node(7, Node(5, Nil, Nil), Nil)
  let t2 = Node(9, Node(7, Nil, Nil), Nil)
  Assert.AreEqual(map ((+) 2) t1, t2)

[<Test>]
let ``Fold (-) of Nil`` () =
  Assert.AreEqual(fold (-) Nil 0, 0)

[<Test>]
let ``Fold (+) of Node(21, Node(4, Node(3, Nil, Nil), Nil), Node(25, Nil, Node(31, Nil, Nil)))`` () =
  let t = Node(21, Node(4, Node(3, Nil, Nil), Nil), Node(25, Nil, Node(31, Nil, Nil)))
  Assert.AreEqual(fold (+) t 0, 84) 

[<Test>]
let ``Filter (< 6) Nil`` () =
  Assert.AreEqual(filter ((>) 6) Nil, Nil)

[<Test>]
let ``Filter (> 5) Node(19, Node(5, Node(3, Nil, Nil), Nil), Node(25, Nil, Node(21, Nil, Nil)))`` () =
  let t1 = Node(19, Node(5, Node(3, Nil, Nil), Nil), Node(25, Nil, Node(21, Nil, Nil)))
  let t2 = Node(19, Nil, Node(25, Nil, Node(21, Nil, Nil)))
  Assert.AreEqual(filter ((<) 5) t1, t2)


 