module ST = struct
  type 'a st = ST : 's * 'a -> 'a st
  type 'a stref = STRef : 's option * 'a ref -> 'a stref

  let runST (ST (_, x)) = x
  let pure x = ST (None, x)
  let bind (ST (_, x)) (k : 'a -> 'b st) = k x
  let ( let* ) = bind

  let newstref x = ST (None, STRef (None, ref x))
  let readstref (STRef (s, x)) = ST (s, !x)
  let modifystref (STRef (s, rf)) f =
    rf := f !rf ;
    ST (s, ())
end

(* module STRef = struct *)
(*   type 'a stref = STRef : 's * 'a ref -> 'a stref *)
(* end *)
