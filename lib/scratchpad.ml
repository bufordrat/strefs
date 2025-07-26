module ST = struct

  type realworld = RW : 'a -> realworld

  type 'a st = ST : 's * 'a -> 'a st
  type 'a stref = STRef : 's option * 'a ref -> 'a stref

  let runST (ST (_, x)) = x
  let pure x = ST (None, x)
  let bind (ST (_, x)) (k : 'a -> 'b st) = k x

  let newstref x = ST (None, STRef (None, ref x))
  let readstref (STRef (s, x)) = ST (s, !x)
  let modifystref (STRef (s, rf)) f =
    rf := f !rf ;
    ST (s, ())

  (* type 'a st = ST : ('s option, 'a) State.t * 's option -> 'a st *)
  (* let runST (ST (stateful, initial)) = fst (stateful initial) *)

  (* a bit harder than you might think to make this puppy a monad *)

  (* let pure x = *)
  (*   let open State in *)
  (*   let statey s = pure x s in *)
  (*   ST (statey, None) *)

  (* let bind (ST (statey, state1)) k = *)
  (*   let ( result1, state2 ) = statey state1 in *)
  (*   match k result1 with *)
  (*     ST ( statey2, _ ) -> ST (statey2, state2) *)

       (* yeah, kinda got nothing right now for bind *)
  
       (* let bind (ST (statey, state1)) k = *)
       (*   let ( result1, state2 ) = statey state1 *)
       (*   in match k result1 with *)
       (*        ST (statey2, state3) *)

end

(* module STRef = struct *)
(*   type 'a stref = STRef : 's * 'a ref -> 'a stref *)
(* end *)
