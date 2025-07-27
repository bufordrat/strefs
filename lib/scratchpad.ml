type ('s, 'a) st = ST : { st : 's. 'a } -> ('s, 'a) st
type ('s, 'a) stref = STRef of 'a ref

let runST (ST { st = x }) = x
let pure x = ST { st = x }
let bind : type s a b. (s, a) st -> (a -> (s, b) st) -> (s, b) st =
  fun (ST { st = x }) k -> k x
let ( let* ) = bind

let newstref : type s a. a -> (s, (s, a) stref) st =
  fun x -> ST { st = STRef (ref x) }
(* let readstref (STRef rf) = ST !rf *)
(* let modifystref (STRef rf) f = *)
(*   rf := f !rf ; *)
(*   ST () *)
