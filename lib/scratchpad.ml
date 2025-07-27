(* type ('s, 'a) st = ST : (unit -> 'a) -> ('s, 'a) st *)

type ('s, 'a) st = ST of (unit -> 'a)
type ('s, 'a) stref = STRef of 'a ref
type 'a runst = { runst : 's. unit -> ('s, 'a) st }

let pure x = ST (fun () -> x)
let bind (ST k) f = ST (fun () -> f (k ()))
let newstref : type s a. a -> (s, (s, a) stref) st =
  fun x -> ST (fun () -> STRef (ref x))
(* let modifystref (STRef (s, rf)) f = *)
(*   rf := f !rf ; *)
(*   ST (s, ()) *)
let runST x = match x.runst () with ST k -> k ()

(* type 'a runst = { runst : 's. unit -> ('s, 'a) st } *)

(* type 'a stref = STRef : 's option * 'a ref -> 'a stref *)

(* let runST (ST (_, x)) = x *)
(* let pure x = ST (None, x) *)
(* let bind (ST (_, x)) (k : 'a -> 'b st) = k x *)
(* let ( let* ) = bind *)

(* let newstref x = ST (None, STRef (None, ref x)) *)
(* let readstref (STRef (s, x)) = ST (s, !x) *)
(* let modifystref (STRef (s, rf)) f = *)
(*   rf := f !rf ; *)
(*   ST (s, ()) *)

(* type ('s, 'a) st = ST of (unit -> 'a) *)
(* type ('s, 'a) stref = STRef of 'a st *)

(* type 'a st = { runst : 's. unit -> ('s, 'a) st } *)

(* let runST (ST k) = k () *)
(* let pure x = ST (fun () -> x) *)
(* let bind (ST k) f = ST (fun () -> f (k ())) *)
(* let newstref x = ST (fun () -> STRef (ref x)) *)
