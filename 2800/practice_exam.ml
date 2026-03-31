(* Question 1 *)





(* Question 2*)
module type AvgAccumSig = sig
  type t
  val empty : t
  val push : float -> t -> t
  val average : t -> float
  

module AvgAccum : AvgAccumSig = struct
  type t = float list

  let empty = []
  
  let push x xs = x :: xs

  let average xs = 
    if xs = [] then 0.0
    else sum_floats xs /. float_of_int (length xs)
end


(* Part B*)
let prop_avg_accum (xs : float list) : bool =
  match xs with
  | [] -> true  (* The property is trivially true for an empty list *)
  | _ -> AvgAccum.average (push_floats xs AvgAccum.empty) = (sum xs /. float_of_int(length xs))
  
  let prop_avg_accum2 (xs : float list) : bool =
  match xs with
  | [] -> true
  | _  -> AvgAccum.average (push_floats xs AvgAccum.empty) =
          (sum_floats xs /. float_of_int (length xs))

      (* 1. Push all elements of xs onto an empty accumulator
      let final_acc = push_floats xs AvgAccum.empty in
      
      (* 2. Get the average from that accumulator *)
      let acc_avg = AvgAccum.average final_acc in
      
      (* 3. Calculate what the average should be using provided helpers *)
      let expected_avg = sum_floats xs /. float_of_int (length xs) in
      
      (* 4. Compare the results *)
      acc_avg = expected_avg *)

(* 
module AvgAccum : AvgAccumSig = struct
  type t = float list

  let empty = []

  let push (x : float) (acc : t) : t =
    x :: acc

  let average (acc : t) : float =
    match acc with
    | [] -> 0.0
    | _ -> 
        let sum = List.fold_left (+.) 0.0 acc in
        sum /. (float_of_int (List.length acc))
end *)

