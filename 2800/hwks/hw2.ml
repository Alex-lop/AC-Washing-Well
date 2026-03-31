(* If you would like, please write here how many hours you spent on the assignment:
  2 hours
*)

(* Q1 *)
let rec concat (xs : string list) : string =
  match xs with
  | [] -> ""
  | h :: t -> h ^ concat t

(* Q2 *)
let rec alternating (xs : int list) : bool =
  let rec expect_even l =
    match l with
    | [] -> true
    | h :: t -> if h mod 2 = 0 then expect_odd t else false
  and expect_odd l =
    match l with
    | [] -> true
    | h :: t -> if h mod 2 <> 0 then expect_even t else false
  in
  expect_even xs
  
(* Q3 *)

type int_tree =
  | Leaf
  | Node of (int_tree * int * int_tree)

let rec gen_int_tree (depth : int) (bound : int) : int_tree =
  if depth <= 0 then Leaf
  else
    let v = Random.int bound
  in
    let sub = gen_int_tree (depth - 1) bound
  in
  Node (sub, v, sub)

let rec string_of_int_tree (t : int_tree) : string =
  match t with
  | Leaf -> "Leaf"
  | Node (l, v, r) ->
      "(" ^ (string_of_int_tree l) ^ " " ^ (string_of_int v) ^ " " ^ (string_of_int_tree r) ^ ")"

(* Q4 *)

let rec map_int_tree (f : int -> int) (t : int_tree) : int_tree =
  match t with
  | Leaf -> Leaf
  | Node (l, v, r) -> Node (map_int_tree f l, f v, map_int_tree f r)


(* Q5 *)

let rec int_tree_max (t : int_tree) : int option =
  match t with
  | Leaf -> None
  | Node (l, v, r) ->
      let left_max = int_tree_max l in
      let right_max = int_tree_max r in
      (* Helper to find max of options and the current value *)
      Some (List.fold_left max v (List.filter_map (fun x -> x) [left_max; right_max]))

(* Q6 *)

let rec int_tree_min (t : int_tree) : int option =
  match t with
  | Leaf -> None
  | Node (l, v, r) ->
      let left_min = int_tree_min l in
      let right_min = int_tree_min r in
      Some (List.fold_left min v (List.filter_map (fun x -> x) [left_min; right_min]))

let rec sorted (t : int_tree) : bool = 
  match t with
  | Leaf -> true
  | Node (l, v, r) ->
      let left_ok = match int_tree_max l with
        | None -> true
        | Some m -> m <= v
      in
      let right_ok = match int_tree_min r with
        | None -> true
        | Some m -> v <= m
      in
      left_ok && right_ok && sorted l && sorted r
