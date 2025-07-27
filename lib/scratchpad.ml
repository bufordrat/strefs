type ('s, 'a) st = ST : 'a -> ('s, 'a) st
type ('s, 'a) stref = STRef : 'a ref -> ('s, 'a) stref

let runST (ST x) = x
let pure x = ST (None, x)
let bind : type s a b. (s, a) st -> (a -> (s, b) st) -> (s, b) st =
  fun (ST x) k -> k x
let ( let* ) = bind

let newstref : type s a. a -> (s, (s, a) stref) st =
  fun x ->  ST (STRef (ref x))
let readstref (STRef rf) = ST !rf
let modifystref (STRef rf) f =
  rf := f !rf ;
  ST ()
