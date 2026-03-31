(* Fk bro that quiz was not bad at all act just got to apply myself bro *)

(* The definition is solid *)
let rec recurse (n : int) (f : 'a -> 'a) (x : 'a) : 'a =
  if n <= 0
  then x
  else recurse (n - 1) f (f x)

(* The usage that triggers the error *)

(* let ex0 = recurse 4 (fun s -> string_of_int s ^ "!") 4 *)
(* OK think we actually clutched up on quiz*)

(* ex of Queue types idk kinda lost rn*)

(* OK creating a set now*)

type set = list
let empty : 'a set = []

(* Making all basic fxns for set *)
let rec membership (x : 'a) (set : 'a set) : bool
  = match set with
  | [] -> false
  | x' :: s' -> if x' = x then true else membership x s'

(* let add (el : 'a) (s : 'a set) : ; *)