(* Anonymous google form for really js the students

Plan: Talking about Functors and Mutability! 7.2
Recall functors are just functions at the module level
*)

module type SetSig = sig
  type elem
  type t
  val empty : t
  val add : elem -> t -> t
  val mem : elem -> t -> bool

end

module type EqType = sig
  type t
  val equals : t -> t -> bool
end

module ListSet (T : EqType) : (SetSig with type elem = T.t(*THIS IS PRETTY IMPORTANT I THINK*)) = struct
  type elem = T.t
  type t = elem list
  let empty = []
  let add x xs = x :: xs
  let rec mem x xs =
    match xs with
    | [] -> false
    | y :: ys -> if T.equals x y then true else mem x ys
end

module StringEq : EqType = struct
  type t = string
  let equals s1 s2 = String.lowercase_ascii s1 = String.lowercase_ascii s2
end

module StringSet = ListSet(StringEq)

module StringList = struct

module ListEq (T : EqType) : EqType = struct
  
  type t = T.t list
  let rec equals xs ys =
  match xs, ys with
  |[], [] -> true
  | x :: xs' , y :: ys' -> T.equals x y && equals xs' ys'
  | _ , _ -> false
end

module StringListSet = ListSet(ListEq(StringEq))

let _ =
  let xs = StringListSet.(add["a"; "b"] empty) in
  ()




(* Mutability, something that was honestly pretty easy t *)

let add_one (x: int ref) = !x + 1
let f() =
  let x = ref 1 in
  let y = add_one x in
  x := 7;
  print_endline ("y : " ^ string_of_int )
