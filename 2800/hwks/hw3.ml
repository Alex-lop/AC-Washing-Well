
(* If you would like, please write here how many hours you spent on the assignment: 
  4
*)

type entry =
  { key : int; value : string }

type bst =
  | Leaf
  | Node of bst * entry * bst

(**** Q1 ****)

(* Q1.1 *)
let rec has_key (t : bst) (k : int) = 
  match t with 
  | Leaf -> false
  | Node (l, e, r) -> 
      if e.key = k then true 
      else has_key l k || has_key r k

(* Q1.2 *)
let rec has_entry (t : bst) (e : entry) : bool = 
  match t with
  | Leaf -> false
  | Node (l, node_e, r) ->
      if node_e = e then true
      else has_entry l e || has_entry r e

(* Q1.3 *)
(* Helper functions to check bounds for sortedness *)
let rec for_all_keys (p : int -> bool) (t : bst) : bool = 
  match t with
  | Leaf -> true
  | Node (l, e, r) -> p e.key && for_all_keys p l && for_all_keys p r

let rec sorted (t : bst) : bool = 
  match t with
  | Leaf -> true
  | Node (l, e, r) -> 
      sorted l && 
      sorted r && 
      for_all_keys (fun k -> k <= e.key) l && 
      for_all_keys (fun k -> k >= e.key) r


(**** Q2 ****)

(* Helper to generate a random entry *)
let gen_random_entry () = 
  { key = Random.int 20; value = "val" ^ string_of_int (Random.int 100) }

let rec gen_bst (n : int) : unit -> bst = 
  fun () ->
    if n <= 0 || Random.bool () then Leaf
    else 
      let l = gen_bst (n - 1) () in
      let r = gen_bst (n - 1) () in
      let e = gen_random_entry () in
      Node (l, e, r)

(**** Q3 ****)
let rec forall_bst
    (gen_bst : unit -> bst)
    (prop : bst -> bool)
    (trials : int)
  : bst option = 
  if trials <= 0
  then None
  else 
    let x = gen_bst () in 
    if prop x
    then forall_bst gen_bst prop (trials - 1)
    else Some x

let rec forall_bst_entry
    (gen_bst : unit -> bst)
    (gen_entry : unit -> entry) 
    (prop : bst * entry -> bool)
    (trials : int)
  : (bst * entry) option = 
  if trials <= 0
  then None
  else 
    let x = gen_bst () in 
    let y = gen_entry () in 
    let xy = (x, y) in
    if prop xy
    then forall_bst_entry gen_bst gen_entry prop (trials - 1)
    else Some xy

let rec forall_bst_key
    (gen_bst : unit -> bst)
    (gen_key : unit -> int) 
    (prop : bst * int -> bool)
    (trials : int)
  : (bst * int) option = 
  if trials <= 0
  then None
  else 
    let x = gen_bst () in 
    let y = gen_key () in 
    let xy = (x, y) in
    if prop xy
    then forall_bst_key gen_bst gen_key prop (trials - 1)
    else Some xy

(* Q3.1 *)
(* Performs a pre-order traversal to find the first matching key *)
let rec lookup (t : bst) (k : int) : string option = 
  match t with
  | Leaf -> None
  | Node (l, e, r) ->
      if e.key = k then Some e.value
      else 
        match lookup l k with
        | Some v -> Some v
        | None -> lookup r k

(* Q3.2 *)
let p1 () : (bst * int) option = 
  let gen_key () = Random.int 20 in
  let prop (t, k) = 
    if lookup t k = None then not (has_key t k) else true
  in
  forall_bst_key (gen_bst 5) gen_key prop 1000


(* Q3.3 *)

(* This property is false, so it should return a counter-example. *)
let p2 () : (bst * entry) option = 
  let prop (t, e) = 
    if has_entry t e then lookup t e.key = Some e.value else true
  in
  forall_bst_entry (gen_bst 5) gen_random_entry prop 1000

(* Briefly explain below why this counter-example breaks the property:

   In an unsorted BST, duplicates are allowed. The lookup function performs a linear 
   search (e.g., pre-order). If the tree contains two entries with the same key but 
   different values (e.g., root has (1, "A") and left child has (1, "B")), 
   has_entry is true for (1, "B"), but lookup will find (1, "A") first and return "A". 
   Since "A" <> "B", the property fails.
*)
let p2_counter_example () : (bst * entry) = 
  let e1 = { key = 1; value = "A" } in
  let e2 = { key = 1; value = "B" } in
  let t = Node (Node (Leaf, e2, Leaf), e1, Leaf) in
  (t, e2)


(* Q3.4 *)

(* Why is (P2') true? 

   Even if there are duplicate keys with different values in the tree, if has_entry t e
   is true, then the key e.key definitely exists in the tree. Therefore, lookup t e.key
   will find *some* entry with that key and return Some value, so it will never equal None.
*)
let p2' () : (bst * entry) option = 
  let prop (t, e) = 
    if has_entry t e then lookup t e.key <> None else true
  in
  forall_bst_entry (gen_bst 5) gen_random_entry prop 1000

(**** Q4 ****)
(* Q4.1 *)

let rec lookup_sorted (t : bst) (k : int) : string option = 
  match t with
  | Leaf -> None
  | Node (l, e, r) ->
      if k = e.key then Some e.value
      else if k < e.key then lookup_sorted l k
      else lookup_sorted r k

(* Q4.2 *)
let p3 () : (bst * int) option = 
  let gen_key () = Random.int 20 in
  let prop (t, k) = 
    if lofokup_sorted t k = None then not (has_key t k) else true
  in
  forall_bst_key (gen_bst 5) gen_key prop 1000

  let p3 () : (bst * int) option = 
  let gen_key () = Random.int 20 in
  let prop (t, k) = 
    (* Property: sorted t implies (has_key t k implies lookup_sorted t k <> None) *)
    if sorted t then
      (if has_key t k then lookup_sorted t k <> None else true)
    else true
  in
  forall_bst_key (gen_bst 5) gen_key prop 1000

(* Q4.3 *)
(* Helper to insert into a BST maintaining sorted property *)
let rec insert_sorted (k, v) t = 
  match t with
  | Leaf -> Node (Leaf, {key=k; value=v}, Leaf)
  | Node (l, e, r) ->
      if k < e.key then Node (insert_sorted (k, v) l, e, r)
      else Node (l, e, insert_sorted (k, v) r)

let rec gen_sorted (n : int) : unit -> bst = 
  fun () ->
    (* Generate a random number of elements based on depth bound *)
    let num_elements = Random.int (n * 3 + 1) in
    let rec build i acc = 
      if i = 0 then acc
      else 
        let k = Random.int 50 in
        let v = "val" ^ string_of_int k in
        build (i - 1) (insert_sorted (k, v) acc)
    in
    build num_elements Leaf

(* Q4.4 *)
let gen_sorted_property () : bst option = 
  forall_bst (gen_sorted 10) sorted 1000

(* Q4.5 *)
let p3_gen_sorted () : (bst * int) option =
  let gen_key () = Random.int 50 in
  let prop (t, k) =
    if lookup_sorted t k = None then not (has_key t k) else true
  in
  forall_bst_key (gen_sorted 10) gen_key prop 1000

  let p3_gen_sorted () : (bst * int) option =
  let gen_key () = Random.int 50 in
  let prop (t, k) =
    if sorted t then
      (* Use '=' to represent 'iff' *)
      (has_key t k) = (lookup_sorted t k <> None)
    else 
      true (* Vacuously true if not sorted *)
  in
  forall_bst_key (gen_sorted 10) gen_key prop 1000

(* Q4.6 *)
let p4 () : (bst * int) =
  (* Construct a tree that is NOT sorted where a key exists in the "wrong" branch.
Root is 10. Key 5 is in the Right branch (should be Left if sorted).
lookup_sorted 10 5 -> sees 5 < 10, goes Left -> hits Leaf -> returns None.
has_key t 5 -> finds 5 in Right branch -> returns True.
     Property (None -> not True) === (True -> False) === False. *)
  let root = { key = 10; value = "root" } in
  let wrong_child = { key = 5; value = "hidden" } in
  let t = Node (Leaf, root, Node(Leaf, wrong_child, Leaf)) in
  (t, 5)