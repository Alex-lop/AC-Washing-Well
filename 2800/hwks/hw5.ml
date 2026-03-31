(* If you would like, write how many hours you spent on this homework:

  3

*)

(* Here we are showcasing an optional argument in ocaml: *)
let rec forall
    ?(ntrials : int = 1000)   (* declaring an optional argument with a type *)
    (gen : unit -> 'a)
    (prop : 'a -> bool)
  : 'a option
  = if ntrials <= 0
    then None
    else
      let x = gen () in 
      if prop x
           (* applying an optional argument, using the ~ntrials: label *)
      then forall ~ntrials:(ntrials - 1) gen prop 
      else Some x

let gen2 f g : unit -> ('a * 'b) = fun _ -> (f (), g ())
let gen3 f g h : unit -> ('a * 'b * 'c) = fun _ -> (f (), g (), h ())
let gen4 f g h i : unit -> ('a * 'b * 'c * 'd) = fun _ -> (f (), g (), h (), i ())

(* ** Q0 ** *)

module type IntStackSig = sig
    type t 
    val empty : t 
    val push : int -> t -> t 
    val peek : t -> int option 
    val pop : t -> t
end

module IntStack : IntStackSig = struct
    type t = int list
    let empty = []
    let push x xs = x :: xs
    let peek xs = 
      match xs with
      | [] -> None
      | x :: _ -> Some x
    let pop xs = 
      match xs with
      | [] -> []
      | _ :: xs' -> xs'
end

let gen_int () = Random.int 100

(* note that these functions use "mutually recursive" syntax *)
let rec gen_intstack n gen_int : unit -> IntStack.t =
  fun () -> gen_intstack_rec n gen_int IntStack.empty

and gen_intstack_rec (n : int) (g : unit -> int) (acc : IntStack.t) : IntStack.t = 
  if n <= 0 then acc else 
  gen_intstack_rec (n - 1) g (IntStack.push (g ()) acc)

let prop_peek_push (x, s : int * IntStack.t) : bool =
  IntStack.peek (IntStack.push x s) = Some x

(* P1: Popping a stack after pushing an element returns the original stack. *)
let prop1_pop_push (x, s : int * IntStack.t) : bool =
  IntStack.pop (IntStack.push x s) = s

(* P2: Peeking at an empty stack always returns None. *)
let prop2_peek_empty () : bool =
  IntStack.peek IntStack.empty = None

let stack_spec () : bool =
     (forall (gen2 gen_int (gen_intstack 20 gen_int)) prop_peek_push = None)
  && (forall (gen2 gen_int (gen_intstack 20 gen_int)) prop1_pop_push = None)
  && (forall (fun () -> ()) (fun _ -> prop2_peek_empty ()) = None)


(** Q1 **)

type 'a nelist = | First of 'a | Cons of 'a * 'a nelist
module type NonEmptySig = sig
    type 'a t
    val well_formed : 'a t -> bool
    val mk : 'a -> 'a t
    val cons : 'a -> 'a t -> 'a t
    val uncons : 'a t -> ('a * ('a t) option) 
    val to_list : 'a t -> 'a list
end

module NonEmpty = struct
    type 'a t = 'a list

    (* INVARIANT: xs has length > 0 *)
    let well_formed xs = 
        match xs with
        | [] -> false
        | _ :: _ -> true

    let mk x = [x]
    let cons x xs = x :: xs
    let uncons xs = 
        match xs with
        | [] -> failwith "UNREACHABLE"
        | [x] -> (x, None)
        | x :: xs' -> (x, Some xs')
    let to_list xs = xs
end

(*
    Q1.1: Explain why the invariant is maintained for each of the following
    functions, and UNREACHABLE should never be reached:
    - mk: Creates a list [x], which has a length of 1, satisfying length > 0.
    - cons: If xs has length n > 0, then x :: xs has length n + 1, which is also > 0.
    - uncons: If well_formed holds, xs is never [], so the pattern match for [] is never reached.
*)

(* Q1.2: 
    - map: Can be implemented. Mapping a function over a list preserves the length; if the input is non-empty, the output is non-empty.
    - filter: Cannot be implemented. A predicate might return false for every element, resulting in an empty list. To fix this, it should return an 'a t option.
*)


(** Q2 **)

type 'a raw_tree = | Leaf of 'a | Node of 'a raw_tree * 'a raw_tree

module type BalancedTreeSig = sig
    (* The type for balanced trees. *)
    type 'a t

    (* Is the balanced tree well-formed? *)
    val well_formed : 'a t -> bool

    (* Build the balanced tree consisting of a single leaf. *)
    val leaf : 'a -> 'a t

    (* Build the balanced tree consisting of a node (or returning None if it would result in a non-balanced tree.) *)
    val node : 'a t -> 'a t -> 'a t option

    (* Return the underlying raw_tree. *)
    val to_tree : 'a t -> 'a raw_tree
end

module BalancedTree : BalancedTreeSig = struct
    type 'a t = 'a raw_tree * int

    let rec get_depth (tr : 'a raw_tree) : int option =
      match tr with
      | Leaf _ -> Some 0
      | Node (l, r) ->
          match get_depth l, get_depth r with
          | Some dl, Some dr when dl = dr -> Some (dl + 1)
          | _ -> None

    let well_formed (tr, d) = 
      match get_depth tr with
      | Some d' -> d = d'
      | None -> false

    let leaf x = (Leaf x, 0)

    let node (t1, d1) (t2, d2) = 
      if d1 = d2 then Some (Node (t1, t2), d1 + 1) else None

    let to_tree (tr, _) = tr
end