(* Workflow for ring
(expectation: 2 h; reality: 1,5 h)
by Sokolova Polina *)

module RingWorkflow
  
type RingBuilder (n : int) =
  member x.Bind (a, f) = (f a) % n
  member x.Return a = 
    let rec ret b =
      if b < 0 then ret (b + n)
      else b
    ret a % n

let ring n = new RingBuilder (n)



