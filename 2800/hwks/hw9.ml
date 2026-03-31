(* If you would like, write how many hours you spent on this homework:

  4

*)

(** Q1 **)

module Fresh = struct
  let ctr = ref 0
  let fresh () = 
      ctr := !ctr + 1;
      !ctr
end

module type FreshSig = sig
  val fresh : unit -> int
end

module FixedFresh : FreshSig = struct 
  let ctr = ref 0
  let fresh () = 
      ctr := !ctr + 1;
      !ctr
end

(* Return two integers --- both coming from different calls to Fresh.fresh --- that should always be equal. *)
let break_fresh () : int * int = 
  let first = Fresh.fresh () in
  Fresh.ctr := !Fresh.ctr - 1; 
  let second = Fresh.fresh () in
  (first, second)

(* TODO: Why does the above code not work for FixedFresh? 

  The reason we can get the same value when calling Fresh.fresh but not for FixedFresh is because the module "Fresh" does not have a module type (signature) restricting its 
  contents, so its internal mutable reference "ctr" is visible and 
  modifiable by any code outside the module

  However, for FixedFresh, because we have a defined module that only exposes fresh, we cannot modify the internal mutable reference "ctr".

*)

(** Q2 **)

module type ManualRefSig = sig
  type 'a arena 
  type 'a mref

  (* Create a new arena. The argument (capacity of the arena) must be nonnegative. *)
  val fresh_arena : int -> 'a arena

  (* Find an unused slot in the arena and store the given value there; and return an mref to that location. 
     Return None if no free space is available.
  *)
  val alloc : 'a arena -> 'a -> 'a mref option

  (* Free the mref, which should allow that slot in the arena to be re-used. If the mref points to an already freed location, the operation should do nothing. *)
  val free : 'a arena -> 'a mref -> unit

  (* Read an mref. Should return None if the mref is pointing to a freed location in the arena. *)
  val read : 'a arena -> 'a mref -> 'a option

  (* Write to an mref. Should return None if the mref is pointign to a freed location in the arena. *)
  val write : 'a arena -> 'a mref -> 'a -> unit option

  (* Do the two mrefs point to the same index in the arena? *)
  val phys_eq : 'a mref -> 'a mref -> bool

  (* Return the number of currently free cells in the arena. *)
  val num_free : 'a arena -> int

  (* Return the number of times we have called alloc on the arena. *)
  val num_allocations : 'a arena -> int
end

module ManualRef : ManualRefSig = struct
    type 'a arena = {
      slots : 'a option array;
      mutable total_allocs : int;
      mutable current_free : int;
    }
    type 'a mref = int

    let fresh_arena (n : int) = 
      { slots = Array.make n None; total_allocs = 0; current_free = n }

    let alloc (a : 'a arena) (x : 'a) =
      a.total_allocs <- a.total_allocs + 1;
      let rec find i =
        if i >= Array.length a.slots then None
        else if a.slots.(i) = None then (
          a.slots.(i) <- Some x;
          a.current_free <- a.current_free - 1;
          Some i
        ) else find (i + 1)
      in find 0

    let free (a : 'a arena) (m : 'a mref) =
      if m >= 0 && m < Array.length a.slots then
        match a.slots.(m) with
        | Some _ -> 
            a.slots.(m) <- None; 
            a.current_free <- a.current_free + 1
        | None -> ()

    let read (a : 'a arena) (m : 'a mref) =
      if m >= 0 && m < Array.length a.slots then a.slots.(m) else None

    let write (a : 'a arena) (m : 'a mref) (x : 'a) =
      if m >= 0 && m < Array.length a.slots then
        match a.slots.(m) with
        | Some _ -> a.slots.(m) <- Some x; Some ()
        | None -> None
      else None

    let phys_eq (m1 : 'a mref) (m2 : 'a mref) = m1 = m2
    let num_free (a : 'a arena) = a.current_free
    let num_allocations (a : 'a arena) = a.total_allocs
end

(** Q3 **)

module MemorySpecs (R : ManualRefSig) = struct
  open R
  let leakage_free (a : 'a arena) (f : 'a arena -> 'b) : bool = 
    let before = num_free a in
    let _ = f a in
    num_free a = before

  let num_allocations_in (a : 'a arena) (f : 'a arena -> 'b) : int = 
    let before = num_allocations a in
    let _ = f a in
    num_allocations a - before
end

(** Q4 **)

module Fib (R : ManualRefSig) = struct
  open R
  module IntMap = Map.Make(Int)

  (* ... Helper functions provided in prompt ... *)
  let create_map (n : int) (f : int -> 'a) : 'a IntMap.t = 
      IntMap.of_list (List.init n (fun i -> (i, f i)))

  let create_map_option (n : int) (f : int -> 'a option) : ('a IntMap.t) option = 
    let pairs = List.init n (fun i -> (i, f i)) in 
    if List.exists (fun p -> (Option.is_none (snd p))) pairs then 
        None 
    else 
        Some (IntMap.of_list (List.map (fun p -> (fst p, Option.get (snd p))) pairs))

  let iter_map (m : 'a IntMap.t) (f : int -> 'a -> unit) : unit = 
      IntMap.iter f m

  (* Implementation of Memoized Fib using the Arena *)
  let fib_fast (a : int arena) (n : int) : int option =
    if n < 0 then None
    else if n = 0 then Some 0
    else if n = 1 then Some 1
    else
      (* Create a memoization table (Map) where each entry is an mref in the arena *)
      (* We initialize them with -1 to indicate 'not yet calculated' *)
      let memo_map_opt = create_map_option (n + 1) (fun _ -> alloc a (-1)) in
      match memo_map_opt with
      | None -> None (* Not enough space in arena *)
      | Some memo_map ->
          let rec compute k =
            if k = 0 then 0
            else if k = 1 then 1
            else
              match read a (IntMap.find k memo_map) with
              | Some v when v <> -1 -> v (* Return cached value *)
              | _ ->
                  let res = compute (k - 1) + compute (k - 2) in
                  let _ = write a (IntMap.find k memo_map) res in
                  res
          in
          let result = Some (compute n) in
          (* Clean up: free all mrefs we allocated for this call *)
          iter_map memo_map (fun _ mref -> free a mref);
          result
end