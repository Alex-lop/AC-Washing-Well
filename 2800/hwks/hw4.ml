(* If you would like, write how many hours you spent on this homework:

  4 Hours

*)

let rec forall gen prop n =
  if n <= 0 then None else
let x = gen () in
  if prop x then forall gen prop (n - 1) else Some x

let gen_pair (f : unit -> 'a) (g : unit -> 'b) : (unit -> ('a * 'b)) = 
    fun _ -> 
        let x = f () in 
        let y = g () in 
        (x, y)


(* ** Q0, warmup ** *)

let prop_duplicate_element (duplicate_element : 'a list -> 'a option) (xs : 'a list) : bool =
  let count x list = List.length (List.filter (fun y -> y = x) list) in
  match duplicate_element xs with
  | Some x -> count x xs > 1
  | None -> List.for_all (fun x -> count x xs <= 1) xs

let duplicate_element (xs : 'a list) : 'a option =
  let sorted = List.sort compare xs in
  let rec find = function
    | x1 :: x2 :: _ when x1 = x2 -> Some x1
    | _ :: t -> find t
    | [] -> None
  in find sorted

(* ** Q1 ** *)

(* *** PROPERTIES GO HERE ***

Property 1: If we push onto the empty queue, then peek, we get the thing we pushed with the same priority. 
------
forall x : string, forall i : int,
    peek (push empty x i) = Some (x, i)

Property 2: When we push two elements onto a queue, and then take the size,
this gives the same result as reversing the pushes.
-----
forall q : queue, forall x : string, forall y : string, forall i : int, forall j : int,
    size (push (push q x i) y j) = 
    size (push (push q y j) x i)


Your properties below:
Property 3: Popping an element from a non-empty queue reduces the size by exactly one.
-
forall q : pqueue, 
    if size q > 0 then size (pop q) = size q - 1 else size q = 0

Property 4: Incrementing a queue increases the priority of the highest priority element by 1.
-
forall q : pqueue,
    match peek q with
    | None -> peek (increment q) = None
    | Some (x, i) -> peek (increment q) = Some (x, i + 1)

*)

(* ** Q5: Implement these functions /after/ finishing Q1-4! *)
type 'a pqueue = ('a * int) list

let empty (() : unit) : 'a pqueue = []

let push (pq : 'a pqueue) (el : 'a) (priority : int) : 'a pqueue =
  (el, priority) :: pq

let peek (pq : 'a pqueue) : ('a * int) option =
  match pq with
  | [] -> None
  | h :: t -> Some (List.fold_left (fun (v1, p1) (v2, p2) ->
      if p2 > p1 then (v2, p2) else (v1, p1)) h t)

let size (pq : 'a pqueue) : int = List.length pq

let pop (pq : 'a pqueue) : 'a pqueue =
  match peek pq with
  | None -> []
  | Some (v, p) ->
      let rec remove_one found = function
        | [] -> []
        | (v', p') :: t when not found && v = v' && p = p' -> t
        | h :: t -> h :: remove_one (h = (v, p) || found) t
      in remove_one false pq

let increment (pq : 'a pqueue) : 'a pqueue =
  List.map (fun (v, p) -> (v, p + 1)) pq

type 'a pq_ops = {
    empty : unit -> 'a pqueue;
    push : 'a pqueue -> 'a -> int -> 'a pqueue;
    peek : 'a pqueue -> ('a * int) option;
    size : 'a pqueue -> int;
    pop : 'a pqueue -> 'a pqueue;
    increment : 'a pqueue -> 'a pqueue;
}

let my_pq_api = {
    empty = empty;
    push = push;
    peek = peek;
    size = size;
    pop = pop;
    increment = increment;
}

(* ** Q2 ** *)

let prop1 (ops : 'a pq_ops) (x : string) (i : int) : bool =
  ops.peek (ops.push (ops.empty ()) x i) = Some (x, i)

let prop2 (ops : 'a pq_ops) (q : 'a pqueue) (x : string) (y : string) (i : int) (j : int) : bool =
  ops.size (ops.push (ops.push q x i) y j)
  =
  ops.size (ops.push (ops.push q y j) x i)

let prop3 (ops : 'a pq_ops) (q : 'a pqueue) : bool =
  let s = ops.size q in
  if s > 0 then ops.size (ops.pop q) = s - 1
  else ops.size q = 0

let prop4 (ops : 'a pq_ops) (q : 'a pqueue) : bool =
  match ops.peek q with
  | None -> ops.peek (ops.increment q) = None
  | Some (x, i) -> ops.peek (ops.increment q) = Some (x, i + 1)

(* ** Q3 ** *)

type 'a instruction = | Push of 'a * int | Pop | Increment

let gen_string () : string =
  String.make 1 (char_of_int (65 + Random.int 26))

let gen_priority () : int = Random.int 100

let rec gen_ins () : string instruction list =
  if Random.int 10 = 0 then []
  else 
    let ins = match Random.int 3 with
      | 0 -> Push (gen_string (), gen_priority ())
      | 1 -> Pop
      | _ -> Increment
    in ins :: gen_ins ()

let rec mk_queue (ops : 'a pq_ops) (xs : 'a instruction list) : 'a pqueue =
  List.fold_left (fun q ins ->
    match ins with
    | Push (v, p) -> ops.push q v p
    | Pop -> ops.pop q
    | Increment -> ops.increment q
  ) (ops.empty ()) xs

(* ** Q4 ** *)
let prop1_test (ops : string pq_ops) () : (string * int) option =
    forall (gen_pair gen_string gen_priority) (fun (x, i) -> prop1 ops x i) 32

let prop2_test (ops : string pq_ops) () : (string instruction list * (string * (string * (int * int)))) option =
    forall (gen_pair gen_ins (gen_pair gen_string (gen_pair gen_string (gen_pair gen_priority gen_priority))))
              (fun (xs, (x, (y, (i, j)))) -> 
                prop2 ops (mk_queue ops xs) x y i j
              ) 32

let prop3_test (ops : string pq_ops) () : (string instruction list) option =
    forall gen_ins (fun xs -> prop3 ops (mk_queue ops xs)) 32

let prop4_test (ops : string pq_ops) () : (string instruction list) option =
    forall gen_ins (fun xs -> prop4 ops (mk_queue ops xs)) 32


(* *** Q6 *** *)
let my_q_correct () : bool =
  prop1_test my_pq_api () = None &&
  prop2_test my_pq_api () = None &&
  prop3_test my_pq_api () = None &&
  prop4_test my_pq_api () = None