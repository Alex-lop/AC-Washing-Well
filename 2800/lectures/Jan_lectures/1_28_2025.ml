(* So basically think about it visually of the nodes w the leafs and everything like w juan*)

type bintree =
  | Leaf of int
  | Branch of bintree * bintree

let rec gen_bintree (leaf_bound : int) (n : int) : bintree =
  match n with
  | i when i <= 0 -> Leaf(Random.int leaf_bound)
  | _ -> Branch (gen_bintree leaf_bound (n - 1), gen_bintree leaf_bound (n - 1))

(* Ok now this is an example using if rather than match
  The return is the main difference as it is a unit returning a bintree (?)*)
let rec gen_bintree2 (leaf_bound : int) (n : int) :  unit -> bintree =
  fun () ->
    if n <= 0
    then Leaf (Random.int leaf_bound)
    else Branch (gen_bintree2 leaf_bound (n - 1) (), gen_bintree2 leaf_bound (n - 1) ())

(* Bintree function now*)
(* let rec bintree_fun (bt : bintree) = failwith 
  match bt with
  | Leaf v -> ... v ...
  | Branch (l, r) *)

(* Function to sum up bintree*)
let rec sum_bintree (bt : bintree) : int =
  match bt with
  | Leaf v -> v
  | Branch (l, r) -> sum_bintree (l) + sum_bintree (r)

type entry = {
  tag : string;
  value : int
}

type tagged_bintree =
| Leaf of entry
| Branch of bintree * bintree


(*So we can write this second form of summing based on some kind of blessed tag that will filter over tree and anytmime we see a bintree (or entry?) with predetermiend....*)

(* let rec collect_tagged (q : string) (bt: bintree) : int list =
  match bt with
  | Leaf {tag = i; value = v} -> if t = q then [v] else []
  | Branch (l, r) -> collect_tagged q l @ collect_tagged q r The @ symbol is a way of concatination for lists Prob important so know it! *)


(* Getting a tagged bintree*)

(* let gen_tag (xs : string list) : unit -> string =
  fun () -> List.nth xs (Random.int (List.length xs)) (* pretty much just getting a ranodm  entry in a list you input*)
    

let gen_entry : unit -> entry =
  fun () -> { tag = gen_tag () ; value = Random.int 100}

let rec gen_tagged_bintree (n : int) :  unit -> tagged_bintree =
  fun () ->
    if n <= 0
    then Leaf (gen_entry ())
    else Branch (gen_tagged_bintree (n' - (1 + Random.int 2) ()),
    gen_tagged_bintree (n - (1 +Random.int 2) ())) *)



(*Start of 1/29/2025*)
(* let ex0 = gen_tagged_bintree 10 () *)

(* let rec sum (xs : int list) : int = 
  match xs with
  |[] -> 0
  | x :: xs -> x + sum xs

  let sum_tagged (q : string) (bt : tagged_bintree) : int = sum (collect_tagged q bt)
  let sum_reference = (q : string) (bt : tagged_bintree) : int = failwith ".."

  let prop_sum_is_sum_tagged = (sum_tagged : string -> tagged_bintree -> int) : string -> tagged_bintree -> bool = 
    fun q bt ->
      sum_tagged q bt = sum_reference q bt *)

(* let rec forall_tagged_bintree
(gen_bt :)

Just a copy and pasted it, *)

(* let rec append_ints (xs : int list) (ys: int list) : int list =
  match xs with
  | [] -> ys
  | (::) (x, xs') -> s :: append_ints xs' ys *)

(* This is where polymorphism comes in useful so we don't have to just copy and paste code for every append_bool, strings .. etc .etc*)

(* This version works for any type 'a *)
let rec append_generic (xs : 'a list) (ys : 'a list) : 'a list =
  match xs with
  | [] -> ys
  | x :: xs' -> x :: append_generic xs' ys

(* let rec map (f: 'a -> 'b) (xs : 'a list) : list =
  match xs with
  | [] -> []
  | x :: xs ->  *)

(* things to run in utop to understand type variables
NEED to act understadn this its not that bad
let f b c d = c (d b)
fun a b c d -> d(c a b)
fun a b -> b < a   (this is a polymorphic fxn I think thats what he said)
fun a b c d -> if d then [b;c] else [a]
*)

