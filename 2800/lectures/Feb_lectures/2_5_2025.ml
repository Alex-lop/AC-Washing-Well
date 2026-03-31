(* This is a polymorphic implementaiton of the example last time, so we can put anything not js list int for ex (useful bc it removes a lot of boiler plate code)*)

module type MySetSig = sig
  type 'a t
  val empty : 'a t
  val mem : 'a -> 'a t -> bool
  val add : 'a -> 'a t -> 'a t
  val remove : 'a -> 'a t -> 'a t
end

module type MySetSig = struct
  type 'a t = 'a list
  let empty : 'a t = []
  let mem : 'a -> 'a t -> bool = list.mem
  let add (x : 'a) (xs : 'a t) : 'a t = x :: xs
  let remove (x : 'a) (xs : 'a t) : 'a t =
    List.filter ((<>) x) xs
end

(* implementing another set instance *)
module FunSet : MySetSig - struct
  type 'a t = 'a -> bool
  let meme(x: 'a) (s : 'a -> bool) = s x
  let empty : 'a -> bool = fun _ -> false
  let add(x: 'a) (s : 'a -> bool) : 'a -> bool = fun x' -> if x = x' then true else s x'
    let remove(x: 'a) (s : 'a -> bool) : 'a -> bool = fun x' -> if x = x' then false else s x'
end

(*  *)
let prop_mem_empty : 'a -> bool
= fun (x : 'a) -> not TestSet.(mem x empty)

let prop_mem_add :  'a * 'a TestSet.t -> bool
= fun (x, s : 'a * 'a TestSet.t) ->
  let open TestSet in
  mem x (add x s)

let prop_mem_remove :
= fun (x, s : 'a & 'a TestSet.t) ->
  not mem x (remove x s)

let rec build_set_acc (n : int) (acc : int testSet.t) : unti-> int testSet.t
= fun() ->
  if n <= 0
    then acc
else (build_set_acc (n-1) (TestSet.add (Random.int 10) acc) ())

let gen_inset (n : int) : unit -> int TestSet.t

let gen_int : unit -> int = fun() -> Random.int 1000