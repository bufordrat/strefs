module ST = struct

  module State = struct
    type ('s, 'a) t = 's -> 'a * 's
    let pure x state = (x, state)
    let bind mx k state1 =
      let ( result1, state2 ) = mx state1
      in k result1 @@ state2
    let ( let* ) = bind
  end

  type 'a st = ST : ('s option, 'a) State.t * 's option -> 'a st
  let runST (ST (stateful, initial)) = fst (stateful initial)

  (* a bit harder than you might think to make this puppy a monad *)

  let pure x =
    let open State in
    let statey s = pure x s in
    ST (statey, None)

  (* yeah, kinda got nothing right now for bind *)
  
(* let bind (ST (statey, state1)) k = *)
  (*   let ( result1, state2 ) = statey state1 *)
  (*   in match k result1 with *)
  (*        ST (statey2, state3) *)

end

module STRef = struct
  type 'a stref = STRef : 's * 'a ref -> 'a stref
end
