type ('s, 'a) st = ST of (unit -> 'a)
type 'a run = { run : 's. unit -> ('s, 'a) st }
type ('s, 'a) stref = STRef of 'a ref

let runST x = match x.run () with
  | ST f -> f ()
let newstref : type s a. a -> (s, (s, a) stref) st =
  fun x -> ST (fun () -> (STRef (ref x)))

(* let x = newstref 12 *)
(* let runx = { run = fun () -> newstref 12 } *)

(* let pure x = ST x *)
(* let bind : type s a b. (s, a) st -> (a -> (s, b) st) -> (s, b) st = *)
(*   fun (ST { st = x }) k -> k x *)
(* let ( let* ) = bind *)


(* let readstref (STRef rf) = ST !rf *)
(* let modifystref (STRef rf) f = *)
(*   rf := f !rf ; *)
(*   ST () *)
