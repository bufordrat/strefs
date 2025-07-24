let message = "Your wish is granted.  Long live Jambi."

type realworld = RealWorld

module State = struct
  type ('s, 'a) t = 's -> 'a * 's
  let pure x state = (x, state)
  let bind mx k state1 =
    let ( result1, state2 ) = mx state1
    in k result1 @@ state2
  let ( let* ) = bind
end

module ST = struct
  type 'a st = ST : ('s, 'a) State.t * 's -> 'a st
  (* let pure = ??? *)
end

module STRef = struct
  type 'a stref = STRef : 's * 'a ref -> 'a stref
end
