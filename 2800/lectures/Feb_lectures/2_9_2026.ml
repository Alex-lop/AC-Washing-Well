(* *)
module type BSig = sig
  type t
  val tru : t
  val fls : t
  val ite : t -> 'a -> 'a -> 'a
end

module B : BSig = struct
  type t = unit option
  let tru : t = None
  let fls : t = Some ()
  let ite b (t : 'a) (f : 'a) : 'a =
    match b with
    | None -> t
    | Some () -> f
end

let t_of_bool (b : bool) : B.t
  = if b then B.tru else B.fls

let bool_of_t (b : B.t) : bool =
  B.ite b true false

module type NatSig = sig
  type t
  val z : t
  val s : t -> t
  (* val to_int : t -> int
  val fr_int : int-> t *)
  val recurse : t -> 'a -> ('a -> 'a) -> 'a
end

module IntNat : NatSig = sig
  type t = int
  let z = 0
  let s : int -> int = fun x -> x + 1
  let recurse (x : t) (v : 'a) (f : 'a -> 'a)  : 'a
  = if x <= 0
    then v
    else f(recurse (x - 1) v f)
end

module Nat (* : NatSig *) = struct
  type t = | Z | S of t
  let z : t = Z
  let s : t -> t = fun x -> S x
  let rec recurse (x : t) (v : 'a) (f : 'a -> 'a) : 'a =
  match x with
  | Z -> v
  | S prev -> f (recurse prev 'x v)
end
(* module Nat (* : NatSig *) = struct
  type t = Z | S of t
  
  let z : t = Z
  
  let s : t -> t = fun x -> S x
  
  let rec recurse (x : t) (v : 'a) (f : 'a -> 'a) : 'a =
    match x with
    | Z -> v
    | S prev -> f (recurse prev v f)
end *)

let add (n : Nat.t) (m : Nat.t) : Nat.t =
Nat.recurse n m Nat.s

module type EvenSig = sig
  type t
  (* mod2 = 0 *)
  (* val mk : int -> t option *)

  val zero : t
  val two : t
  val add : t ->  -> t
  val mult : t -> int -> t
  val get : t -> int
end

module type EvenSig = struct
  type t = int
  (* mod2 = 0 *)
  (* val mk : int -> t option *)

  let zero : t = 0
  let two : t = 2
  let add : t -> t -> t = (+)
  let mult : t -> int -> t = fun x y -> x * y
  let get : t -> int = fun x -> x
end

let prop_is_even (x : Even.t) : bool =
  (Even.get x) mod 2 = 0

let rec