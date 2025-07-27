type ('s, 'a) st = ST : 'a -> ('s, 'a) st
type ('s, 'a) stref = STRef : 'a ref -> ('s, 'a) stref

let runST (ST (_, x)) = x
let pure x = ST (None, x)
let bind : type s a b. (s, a) st -> (a -> (s, b) st) -> (s, b) st =
  fun (ST x) k -> k x
let ( let* ) = bind

(* let newstref x = ST (None, STRef (None, ref x)) *)
(* let readstref (STRef (s, x)) = ST (s, !x) *)
(* let modifystref (STRef (s, rf)) f = *)
(*   rf := f !rf ; *)
(*   ST (s, ()) *)
