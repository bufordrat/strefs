type ('s, 'a) st = ST of (unit -> 'a)
type ('s, 'a) stref = STRef of 'a ref
type 'a stthunk = { stthunk : 's. unit -> ('s, 'a) st }

let pure x = ST (fun () -> x)
let bind : type s a b. (s, a) st -> (a -> (s, b) st) -> (s, b) st = 
  fun (ST x) k -> k (x ())
let newstref : type s a. a -> (s, (s, a) stref) st =
  fun x -> ST (fun () -> STRef (ref x))
let modifystref (STRef rf) f =
  rf := f !rf ;
  ST (fun () -> ())
let readstref : type s a. (s, a) stref -> (s, a) st =
  fun (STRef rf) -> ST (fun () -> !rf)
let runst x = match x.stthunk () with ST k -> k ()
