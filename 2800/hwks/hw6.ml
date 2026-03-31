(* If you would like, write how many hours you spent on this homework:

  XXX

*)

(* this is a module including many PBT functions we have seen up till this point. *)
module Pbt = struct
  (* Main loop of PBT. `forall gen prop num_trials` will test, for `num_trials` 
     number of iterations, if `prop` is true for values generated from `gen`. *)
  (* val forall : (unit -> 'a) -> ('a -> bool) -> int -> 'a option *)
  let forall ?(trials : int = 1000) (gen : unit -> 'a) (prop : 'a -> bool) : 'a option = 
    let rec loop n =
      if n <= 0 then None else 
        let x = gen () in
        if prop x then loop (n - 1) else Some x
    in
    loop trials
  
  (* Pair two generators together. *)
  (* val gen_pair : (unit -> 'a) -> (unit -> 'b) -> (unit -> ('a * 'b))*)
  let gen_pair (gen_a : unit -> 'a) (gen_b : unit -> 'b) : unit -> ('a * 'b) = 
    fun () -> (gen_a (), gen_b ())
  
  
  (* Combine 3 generators together. *)
  (* val gen_triple : (unit -> 'a) -> (unit -> 'b) -> (unit -> 'c) 
                   -> unit -> ('a * 'b * 'c)*)
  let gen_triple (gen_a : unit -> 'a) (gen_b : unit -> 'b) (gen_c : unit -> 'c) : unit -> ('a * 'b * 'c) = 
    fun () -> (gen_a (), gen_b (), gen_c ())
  
  let gen2 = gen_pair
  let gen3 = gen_triple
  let gen4 gen_a gen_b gen_c gen_d = (gen_a (), gen_b (), gen_c (), gen_d ())
  
  (* A generator for lists of elements produced by the supplied generator. *)
  (* val gen_list : (unit -> 'a) -> unit -> 'a list *)
  let gen_list (gen_elem : unit -> 'a) : unit -> 'a list = 
    fun () -> List.(init (Random.int 20) (fun _ -> gen_elem ()))
  
  (* A generator for integers *)
  (* val gen_int : unit -> int *)
  let gen_int () : int = Random.int 100
  
  let gen_bool : unit -> bool = Random.bool

  (* potentially useful for GenTruthVal *)
  let gen_choose a b = if Random.bool () then a () else b ()
  let gen_choose4 a b c d =
    if Random.bool ()
    then gen_choose a b
    else gen_choose c d
  
  let test_assert (name : string) (x : bool) : unit =
    assert (if not x then print_endline (name ^ " failed"); x)
end  
open Pbt

(* Formulas of propositional logic (propositions) are naturally represented in 
   OCaml as expressions of type bool: *)

let p1 a b c = a && (b || c)
let p2 a b c = (a && b) || (a && c)

(* We do not have an implication operation, but we can easily implement it either
   as a function, or even as an operator *)

let impl a b = if a then b else true
let (==>) a b = not a || b

(* PBT demonstration *)
let test_equiv_p1_p2 () =
  forall (gen3 gen_bool gen_bool gen_bool) (fun (a, b, c) -> p1 a b c = p2 a b c)

let test_equiv_impl () =
  forall (gen2 gen_bool gen_bool) (fun (a, b) -> impl a b = a ==> b)


(* Question 1 *)
module type TruthVal = sig
  type t

  val tru : t
  val fls : t

  val tand : t -> t -> t
  val tor : t -> t -> t
  val tnot : t -> t
  val tif : t -> 'a -> 'a -> 'a
end


(* Implement TruthVal using booleans *)


module BoolTruthVal : TruthVal = struct
  type t = bool

  let tru  = true
  let fls  = false

  let tand x y = x && y
  let tor  x y = x || y
  let tnot x   = not x
  let tif  x y z = if x then y else z
end

module GenTruthVal (T : TruthVal) = struct
  (* Changed 'maxdepth' to 'n' to match the expected interface *)
  let rec gen ?(n : int = 20) : unit -> T.t =
    fun () ->
      if n <= 0 then
        if Random.bool () then T.tru else T.fls
      else
        (* Use ~n instead of ~maxdepth *)
        let next = gen ~n:(n - 1) in
        match Random.int 6 with
        | 0 -> T.tru
        | 1 -> T.fls
        | 2 -> T.tand (next ()) (next ())
        | 3 -> T.tor (next ()) (next ())
        | 4 -> T.tnot (next ())
        | _ -> T.tif (next ()) (next ()) (next ())
end

(* Question 2 *)

type variable = int

type variable_clause = 
  | Var of variable
  | NotVar of variable

type 'a formula = 'a list
type 'a cnf = ('a formula) list

let variable_upper_bound (cnf : variable_clause cnf) : variable =
  let extract_var = function
    | Var v -> v
    | NotVar v -> v
  in
  (* Flatten all clauses into one list of literals, then find the max variable *)
  List.fold_left (fun acc clause ->
    List.fold_left (fun acc2 lit -> max acc2 (extract_var lit)) acc clause
  ) 0 cnf


(* Question 3 *)

module Eval (T : TruthVal) = struct
  let eval (cnf : T.t cnf) : T.t =
    (* A clause (list of T.t) is satisfied if ANY element is true (OR) *)
    (* The CNF (list of clauses) is satisfied if ALL clauses are true (AND) *)
    let eval_clause clause = 
      match clause with
      | [] -> T.fls (* Empty clause is unsatisfiable *)
      | x :: xs -> List.fold_left T.tor x xs
    in
    match cnf with
    | [] -> T.tru (* Empty CNF is trivially satisfiable *)
    | c :: cs -> 
        let evaluated_clauses = List.map eval_clause (c :: cs) in
        List.fold_left T.tand (List.hd evaluated_clauses) (List.tl evaluated_clauses)
end

(* Question 4 *)

type 'a mapping = (variable * 'a) list

module Subst(T : TruthVal) = struct
  let subst (vars : T.t mapping) (cnf : variable_clause cnf) : T.t cnf =
    let subst_lit = function
      | Var v -> List.assoc v vars
      | NotVar v -> T.tnot (List.assoc v vars)
    in
    List.map (fun clause -> List.map subst_lit clause) cnf
end

(* Question 5 *)

let all_assignments (vals : 'a list) (bound : variable) : (variable * 'a) list list =
  let open List in
  let rec go n =
    if n <= 0 then [[]] else
      let xs = go (n - 1) in
      let prepend_permutations ys = map (fun v -> (n - 1, v) :: ys) vals in
      concat_map prepend_permutations xs
  in
  go bound

let () =
  test_assert "assignments for 3 variables of a or b value" (
    all_assignments ["a"; "b"] 3 =
    [[(2, "a"); (1, "a"); (0, "a")]; [(2, "b"); (1, "a"); (0, "a")];
     [(2, "a"); (1, "b"); (0, "a")]; [(2, "b"); (1, "b"); (0, "a")];
     [(2, "a"); (1, "a"); (0, "b")]; [(2, "b"); (1, "a"); (0, "b")];
     [(2, "a"); (1, "b"); (0, "b")]; [(2, "b"); (1, "b"); (0, "b")]])

module Sat (T : TruthVal) = struct
  module E = Eval(T)
  module S = Subst(T)

  let sat (cnf : variable_clause cnf) : T.t =
    let bound = variable_upper_bound cnf in
    (* We need to account for the count of variables (bound + 1) *)
    let assignments = all_assignments [T.tru; T.fls] (bound + 1) in
    let results = List.map (fun mapping -> E.eval (S.subst mapping cnf)) assignments in
    match results with
    | [] -> T.tru (* Empty CNF case *)
    | x :: xs -> List.fold_left T.tor x xs
end

(*** Question 6 ****)
module IntTruthVal : TruthVal = struct
  type t = int
  let tru = 1
  let fls = 0
  let tand x y = if x = 1 && y = 1 then 1 else 0
  let tor x y = if x = 1 || y = 1 then 1 else 0
  let tnot x = if x = 1 then 0 else 1
  let tif x y z = if x = 1 then y else z
end

module StringTruthVal : TruthVal = struct
  type t = string
  let tru = "true"
  let fls = "false"
  let tand x y = if x = "true" && y = "true" then "true" else "false"
  let tor x y = if x = "true" || y = "true" then "true" else "false"
  let tnot x = if x = "true" then "false" else "true"
  let tif x y z = if x = "true" then y else z
end

module RecordTruthVal : TruthVal = struct
  type t = { value : unit list }
  let tru = { value = [()] }
  let fls = { value = [] }
  let tand x y = if x.value <> [] && y.value <> [] then tru else fls
  let tor x y = if x.value <> [] || y.value <> [] then tru else fls
  let tnot x = if x.value = [] then tru else fls
  let tif x y z = if x.value <> [] then y else z
end

(* Question 7 *)
module TruthValIso (A : TruthVal) (B : TruthVal) = struct
  let conv (a : A.t) : B.t = A.tif a B.tru B.fls
  let inv (b : B.t) : A.t = B.tif b A.tru A.fls

  let prop_leftinv (a : A.t) = inv (conv a) = a
  let prop_rightinv (b : B.t) = conv (inv b) = b

  module GA = GenTruthVal(A)
  module GB = GenTruthVal(B)

  let check_iso : (A.t * B.t) option =
    (* Providing the label ~n as expected *)
    match forall (GA.gen ~n:20) prop_leftinv with
    | Some a -> Some (a, conv a)
    | None -> (match forall (GB.gen ~n:20) prop_rightinv with
    | Some b -> Some (inv b, b)
    | None -> None)
end