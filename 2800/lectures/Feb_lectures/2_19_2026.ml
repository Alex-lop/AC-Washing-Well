(* Quiz next monday on functors & mutation and mutablilty ?  and invariants ?*)

let test() =
  let f =
    let y = ref 10 in
    fun x->
      y := !y + 1;
      x + !y
  in
  let _ = f 0 in
  f1

let f (x : int ref)  (y : int ref) : int option =
  if x == y
  then None
else (y := 0; Some (!x))

let fwrapper (x : int) (y: int) (alias : bool) : (int option * int * int)
  let (xref, yref) =
  let xref = ref x in
  if alias
    then(xref, xref)
else
  let yref = ref y in
  (xref, yref)
in
let o = (f refx yref) in
(o, !xref, !yref)

let prop_mut (ix , iy, alias) : bool =
  let (o, fx, fy) = fwrapper vs vy alias in
  if alias
    then o = None && ix = fx && iy = fy
else
  math o with
  | None -> false
  |Some v -> v = fx && fy = 0 && fx = ix