(* Q1: Implementing Propositional Logic *)

(* Implement the below function using only &&, ||, and `not`. 
   (You do not need to use all three of these primitives.) 
*)
let implies (p : bool) (q : bool) : bool =
  (not p) || q

(* Next, implement implies using only `if/then/else` expressions -- no `&&`, `||`, or `not`. You can also use `true` and `false`. *)
let implies_if (p : bool) (q : bool) : bool =
  if p then q else true


(* Q2: Translating propositional logic *)

(* (P /\ Q) \/ (not (R /\ S)) *)
let p1 (p : bool) (q : bool) (r : bool) (s : bool) : bool =
  (p && q) || (not (r && s))

(* (P \/ Q) ==> (not R) *)
let p2 (p : bool) (q : bool) (r : bool) : bool =
  implies (p || q) (not r)

(* not (P /\ (Q \/ R)) *)
let p3 (p : bool) (q : bool) (r : bool) : bool =
  not (p && (q || r))

(* (P ==> Q) /\ (Q ==> P) *)
let p4 (p : bool) (q : bool) : bool =
  (implies p q) && (implies q p)

(* Q3: Forall, Exists on Booleans  *)

let forallb (f : bool -> bool) : bool =
  f true && f false

let existsb (f : bool -> bool) : bool =
  f true || f false



(* Q4: Verifying Equivalences *)

(* Calculate the truth value of:
  forall p,
    (p && p) = p
*)
let lemma_1 () : bool =
  forallb (fun p -> (p && p) = p)

(* Calculate the truth value of:
  forall p, exists q,
    (p && q) = p
  *)

let lemma_2 () : bool =
  forallb (fun p -> existsb (fun q -> (p && q) = p))

(* Calculate the truth value of:
  forall p, forall q,
    ((not p) ==> q) = (p \/ q).
*)
let lemma_3 () : bool =
  forallb (fun p -> forallb (fun q -> (implies (not p) q) = (p || q)))

(* Q5: Simplifying Propositions *)

(* Simplify (not p) || q || p, and use forallb to verify that your simplification is correct. *)

let prop_complex (p : bool) (q : bool) : bool =
  (not p) || q || p

let prop_simple (p : bool) (q : bool) : bool =
  true

(* forall p : bool, forall q : bool,  prop_complex p q = prop_simple p q *)
let prop_simple_lemma () : bool =
  forallb (fun p -> forallb (fun q -> prop_complex p q = prop_simple p q))


(* Q5: Forall, Exists on [0..5] *)

(* Do the same as Q3, but instead of over bool, over the set of integers from 0
to 5 (inclusive). *)
let forall_0_5 (f : int -> bool) : bool =
  f 0 && f 1 && f 2 && f 3 && f 4 && f 5

let exists_0_5 (f : int -> bool) : bool =
  f 0 || f 1 || f 2 || f 3 || f 4 || f 5

(* Q6: Formalizing properties on integers *)

(* forall x \in [0..5], exists y \in [0..5], x - y = 0 *)
(* Use forall_0_5 and exists_0_5. *)

let lemma_4 () : bool =
  forall_0_5 (fun x -> exists_0_5 (fun y -> x - y = 0))

(* forall x \in [0..5],
  (x mod 2) = 0 ==> 
    exists k \in [0..5],
      x = 2 * k

  (Hint: mod is an infix operator that can go between numbers in OCaml, 
    just like +, *, or -.)
*)

let lemma_5 () : bool =
  forall_0_5 (fun x ->
    implies (x mod 2 = 0) (exists_0_5 (fun k -> x = 2 * k))
  )