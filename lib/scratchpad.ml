type 'a exist = { e : 's. 's option * 'a }
type 'a st = ST : 'a exist -> 'a st
type 'a stref = STRef : 'a exist -> 'a stref

let runST (ST { e = _, x }) = x
let pure x = ST { e = None, x }
let bind (ST { e = _, x }) (k : 'a -> 'b st) = k x
let ( let* ) = bind

let newstref x = ST { e = None, STRef { e = None, ref x } }
let readstref (STRef { e = s, x }) = ST { e = s, !x }
let modifystref (STRef { e = s, rf }) f =
  rf := f !rf ;
  ST { e = s, () }
