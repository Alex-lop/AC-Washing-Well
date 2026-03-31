 (* Week 11 *)
 (*  Compiling! *)

 type value = int
 type op = | Add | Mul | Or| And | EQ

 type instr =
 | Push of value
 | PrimApp of op
 | Set of string
 | Get of string


 | Jumpe of int
 | JumpIfNotZero of int

let eval_op (op: op) (Args : value list) : value = 
    match (op, Args) with
    | (Add, [x; y]) -> x + y
    | (Mul, [x; y]) -> x * y
    | (Or, [x; y]) -> (if x = 1 || y = 1 then 1 else 0)
    | (And, [x; y]) -> (if x = 1 && y = 1 then 1 else 0)
    | LT , [x; y] -> (if x < y then 1 else 0)
    | (EQ, [x; y]) -> (if x = y then 1 else 0)
    | _ -> failwith "Runtime error"

module StrMap = map.Make(String)

let cfg = instr list * value list & value StrMap.t

let step (cfg : config) : config =

    match cfg with
    | ([], s, h) -> cfg
    | (hd :: tl , s, h) -> match hd with
        |Push v -> (tl, v :: s, h)
        |PrimApp op -> match s with
            | l :: r : rst -> (tl , eval_op op [l; r] :: rst, h)
            | _ -> failwith "Malformed program"
        |Set x -> match s with (tl, List.tl s, StrMap.add x (List.hd s) h)
        |Get x -> 
            match StrMap.find x h with
            | None -> failwith "Variable not found"
            | Some v -> (tl, v :: s, h)
        |Jump n -> (tl, s, h)
        |JumpIfNotZero n -> (tl, s, h)
        | _ -> failwith "Runtime error"

let execute (lst : instr list) : value =
    let rec loop (cfg : cfg) =
        let cfg' = step cfg in
        if cfg' = cfg
        then (match cfg' with
            | ([], hd :: tl, _) -> hd
            | _ -> failwith "Runtime error")
        else  loop cfg'

    in loop (lst, [], StrMap.empty)

let ex0 = [Push 1; Push 2; PrimApp Add]
let crashexample = [Push 1; Push 2; PrimApp Add; Jump 0]







let as_stackvalue (e: Calclang.value) : StackLang.value = 

(* So if we implmented teh jump instruction, we would have to modify the step function to handle the jump instruction. *)
(* We would also have to modify the execute function to handle the jump instruction. *)
(* We would also have to modify the cfg type to handle the jump instruction. *)
(* We would also have to modify the lst type to handle the jump instruction. *)
(* We would also have to modify the execute function to handle the jump instruction. *)
(* We would also have to modify the cfg type to handle the jump instruction. *)
(* We would also have to modify the lst type to handle the jump instruction. *)
(* We would also have to modify the execute function to handle the jump instruction. *)
(* We would also have to modify the cfg type to handle the jump instruction. *)
(* We would also have to modify the lst type to handle the jump instruction. *)



(* OK this lecture is a little over the place but let's go through it anyway *)
module CalcLang = struct
    type value = int
    type expr =
    | Nat of int
    | Bool of bool
    | Add of expr * expr
    | Mul of expr * expr
    | Or of expr * expr
    | And of expr * expr
    | Val v -> v
     Op -> 
    | ITE _ -> failwith "work on this later"

end


let as_stackvalue (e: Calclang.value) : value = 
    let open CalcLang in
    match v with
    | Nat n -> n
    | Bool b -> if b then 1 else 0
    


let next_counter (var : string) (boung : int StrMap.t) : int =
    match StrMap.find var bound with
    | None -> 1
    | Some number_of_occurences -> number_of_occurences + 1
    
(* There comes a problem where if we have a nested expression, we need to keep track of the stack and the heap. *)
(* But because thats a pain in the but, we can use a more efficient way to compile the expression by creating a new pass fxn. *)
let rec uniquify (bound : int StrMap.t) (e : CalcLang.expr) : CalcLang.expr =
    (match e with 
    | Val v -> Val v
    | Var x ->
        match StrMap.find x h with
        | None -> Var x
        | Some number_of_occurences -> Var(x ^ "#" string_of_int number_of_occurences)
    | Let (x, e1 , e2) ->
        let e1` = uniquify bound e1 in
        let bound' = StrMap.add x (next_counter x bound) bound in
        let e2` = uniquify bound e2 in
        Let (x ^ "#" ^ string_of_int n, e1`, e2`)
    | App (op, args) -> App (op, List.map uniquify args)
    )


let rec lower (e : CalcLang.expr) : instr list =
    match e with
    | Val v -> [Push (as_stackvalue v)]
    | Var  x -> [Get x]
    | Let (x, e1 , e2) -> lower e1 @ [Set x] @ e2
    | App (op, args) -> List.flatten(List.map lower (List.rev args) @ [PrimApp op])
let compile (e : CalcLang.expr) : instr list =
    let o = lower e in
