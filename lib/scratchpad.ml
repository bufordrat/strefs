type ('s, 'a) st = ST of (unit -> 'a)
type ('s, 'a) stref = STRef of 'a ref

let pure x = ST (fun () -> x)

let bind : ('s, 'a) st -> ('a -> ('s, 'b) st) -> ('s, 'b) st = 
  fun (ST x) k -> k (x ())

let ( let* ) = bind

let newstref : 'a -> ('s, ('s, 'a) stref) st
  = fun x -> ST (fun () -> STRef (ref x))

let modifystref (STRef rf) f =
  rf := f !rf ;
  ST (fun () -> ())

let readstref : ('s, 'a) stref -> ('s, 'a) st =
  fun (STRef rf) -> ST (fun () -> !rf)

type 'a stthunk = { stthunk : 's. unit -> ('s, 'a) st }

let runst x = match x.stthunk () with ST thunk -> thunk ()

let succ_it =
  let* rf = newstref 0 in
  let* () = modifystref rf succ in
  readstref rf

let succ_it_ref =
  let* rf = newstref 0 in
  let* () = modifystref rf succ in
  pure rf
