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