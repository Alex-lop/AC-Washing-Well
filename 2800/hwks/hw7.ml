(* If you would like, write how many hours you spent on this homework:

3

*)


(* Q1 *)

let mk_counter_1 : unit -> (int -> int) = 
  let c = ref 0 in 
  fun _ -> 
      fun x ->
          c := !c + x;
          !c

let mk_counter_2 : unit -> (int -> int) = 
  fun _ -> 
      let c = ref 0 in 
      fun x ->
          c := !c + x;
          !c

let distingiush (ctr : unit -> int -> int) : int =
  let f1 = ctr () in
  let f2 = ctr () in
  let _ = f1 1 in
  f2 0


(* Q2 *)

(* Q2.1 *)
let f_ok (f : int ref -> int ref -> int) (c : int ref) (d : int ref) : bool =
  let vc = !c in
  let vd = !d in
  let result = f c d in
  result = (vc + vd + 2) && !c = (vc + 1) && !d = (vd + 1)



(* Q2.2 *)

let incr2 (c : int ref) (d : int ref) : int =
  c := !c + 1;
  d := !d + 1;
  (!c + !d)
let incr2_ok = f_ok incr2

(* TODO: Explain why this is a good counter-example. *)
(*This is a good counter-example because incr2 performs the addition
(!c + !d) AFTER the references have been incremented. The
specification requires the return value to be the sum of the
INITIAL values plus 2. When c and d are aliases, the references
are incremented twice, and the final sum reflects those
updated values rather than the starting ones.*)
(* Should call incr2_ok e1 e2, for some expressions e1 and e2. *)
let incr2_counter_example () : bool =
  let r = ref 0 in
  incr2_ok r r

(* Q2.3 *)
let incr2_fixed (c : int ref) (d : int ref) : int =
  let val_c = !c in
  let val_d = !d in
  c := val_c + 1;
  d := val_d + 1;
  (val_c + val_d + 2)



(* Q3 *)
type 'a mtree = MLeaf of 'a | MBranch of ('a mtree) ref * ('a mtree) ref

(* Q3.1 *)
type 'a tree = Leaf of 'a | Branch of 'a tree * 'a tree

let rec mtree_of_tree (t : 'a tree) : 'a mtree =
  match t with
  | Leaf x -> MLeaf x
  | Branch (l, r) -> MBranch (ref (mtree_of_tree l), ref (mtree_of_tree r))

(* Q3.2 *)
let rec mtree_sum (t : int mtree) : int =  match t with
  | MLeaf x -> x
  | MBranch (l, r) -> mtree_sum !l + mtree_sum !r


let my_mtree () : int mtree =
  let r = ref (MLeaf 0) in
  let node = MBranch (r, r) in
  r := node;
  node

(* Q3.3 *)
let mtree_sum_fixed (t : int mtree) : int option = 
  let rec helper (visited : int mtree ref list) (t : int mtree) =
    match t with
    | MLeaf x -> Some x
    | MBranch (l, r) ->
        if List.exists (fun x -> x == l || x == r) visited then None
        else
          let new_visited = l :: r :: visited in
          match helper new_visited !l, helper new_visited !r with
          | Some s1, Some s2 -> Some (s1 + s2)
          | _ -> None
  in helper [] t