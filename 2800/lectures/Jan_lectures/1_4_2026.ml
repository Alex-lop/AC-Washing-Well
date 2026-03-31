(* Example of a foo file, which i think is js a modulo
module type StringSetSig = sig
  type t
  val empty : t
  val mem : string -> t -> t
  val add : string -> t -> t
  val remove : string -> t -> t
end

module HashSet : StringSetSig = struct
  type t = string list
  let empty = []
  let add x xs = Digest.string x :: xs
end *)

