(* If you would like, write how many hours you spent on this homework:

  4

*)

module StringMap = Map.Make(String)

type value = int
type op = | Add | Mul | Sub | Negate | Div | And | Or | LT | EQ | Not

let eval_op (o : op) (vs : value list) : value = 
  match o, vs with
  | Add, [n1; n2] -> (n1 + n2)
  | Mul, [n1; n2] -> (n1 * n2)
  | Sub, [n1; n2] -> (n1 - n2)
  | And, [n1; n2] -> (if n1 = 1 && n2 = 1 then 1 else 0)
  | Negate, [n]   -> (-n)
  | Or, [n1; n2] -> (if n1 = 1 || n2 = 1 then 1 else 0)
  | Div, [n1; n2] -> n1 / n2
  | LT,  [n1; n2] -> (if n1 < n2 then 1 else 0)
  | EQ,  [n1; n2] -> (if n1 = n2 then 1 else 0)
  | Not, [n]      -> (if n <> 1 then 1 else 0)
  | _ -> failwith "eval_op: incorrect arguments"

module type Theory = sig
  type ty
  val type_of_const : value -> ty option
  val type_of_op : op -> ty list -> ty option
  val merge_ty : ty -> ty -> ty option
  val interpret_ty : ty -> value -> bool
end

(* --- Q1: Warmup & IntTheory --- *)

module IntTheory : Theory = struct
  type ty = TInt 

  let type_of_op (o : op) (ts : ty list) : ty option =
    match o, ts with
    | Add, [TInt; TInt] | Mul, [TInt; TInt] | Sub, [TInt; TInt] 
    | And, [TInt; TInt] | Or, [TInt; TInt] | EQ, [TInt; TInt] 
    | LT, [TInt; TInt] -> Some TInt
    | Negate, [TInt] | Not, [TInt] -> Some TInt
    | _ -> None (* Div is disallowed for soundness *)

  let type_of_const (_ : value) : ty option = Some TInt
  let merge_ty (t1 : ty) (t2 : ty) = if t1 = t2 then Some t1 else None
  let interpret_ty (_ : ty) (_ : value) = true
end

module BasicLang (S : Theory) = struct
  type expr = | Value of value | Var of string | Let of string * expr * expr | App of op * expr list | Ite of expr * expr * expr

  let rec eval (m : value StringMap.t) (e : expr) : value = 
    match e with
    | Value v -> v
    | Var x -> StringMap.find x m
    | Let (x, e1, e2) -> eval (StringMap.add x (eval m e1) m) e2
    | App (op, args) -> eval_op op (List.map (eval m) args)
    | Ite (g, t, f) -> if eval m g = 1 then eval m t else eval m f

  let rec typecheck (m : S.ty StringMap.t) (e : expr) : S.ty option = 
    match e with
    | Value v -> S.type_of_const v
    | Var x -> StringMap.find_opt x m
    | Let (x, e1, e2) ->
      (match typecheck m e1 with
       | Some t1 -> typecheck (StringMap.add x t1 m) e2
       | None -> None)
    | App (op, args) ->
      let arg_tys_opt = List.map (typecheck m) args in
      if List.exists Option.is_none arg_tys_opt then None
      else S.type_of_op op (List.map Option.get arg_tys_opt)
    | Ite (g, e1, e2) ->
      match typecheck m g with
      | Some _ -> (match typecheck m e1, typecheck m e2 with
                  | Some t1, Some t2 -> S.merge_ty t1 t2
                  | _ -> None)
      | _ -> None
end

module Q1 = struct 
  open BasicLang(IntTheory)
  let bad_div () : expr = App (Div, [Value 1; Value 0])
end

(* --- Q2: Sign Analysis --- *)

module SignTheory : Theory = struct
  type ty = Pos | Zero | Neg | Any

  let type_of_const (v: value) : ty option =
    if v > 0 then Some Pos else if v = 0 then Some Zero else Some Neg

  let type_of_op (o : op) (ts : ty list) : ty option =
    match o, ts with
    | Add, [Pos; Pos] | Add, [Pos; Zero] | Add, [Zero; Pos] -> Some Pos
    | Add, [Neg; Neg] | Add, [Neg; Zero] | Add, [Zero; Neg] -> Some Neg
    | Add, [Zero; Zero] -> Some Zero
    | Add, [_; _] -> Some Any
    | Mul, [Zero; _] | Mul, [_; Zero] -> Some Zero
    | Mul, [Pos; Pos] | Mul, [Neg; Neg] -> Some Pos
    | Mul, [Pos; Neg] | Mul, [Neg; Pos] -> Some Neg
    | Mul, [_; _] -> Some Any
    | Sub, [t1; t2] -> 
        (match t1, t2 with
        | Pos, Neg | Pos, Zero | Zero, Neg -> Some Pos
        | Neg, Pos | Neg, Zero | Zero, Pos -> Some Neg
        | Zero, Zero -> Some Zero
        | _, _ -> Some Any)
    | Negate, [Pos] -> Some Neg
    | Negate, [Neg] -> Some Pos
    | Negate, [Zero] -> Some Zero
    | Negate, [Any] -> Some Any
    | Div, [_; Zero] | Div, [_; Any] -> None
    | Div, [Zero; (Pos|Neg)] -> Some Zero
    | Div, [(Pos|Neg); (Pos|Neg)] -> Some Any 
    | LT, [Neg; Zero] | LT, [Neg; Pos] | LT, [Zero; Pos] -> Some Pos
    | LT, [Zero; Zero] | LT, [Pos; Neg] | LT, [Pos; Zero] -> Some Zero
    | EQ, [t1; t2] when t1 = t2 && t1 <> Any -> Some Pos
    | EQ, [Pos; Neg] | EQ, [Neg; Pos] | EQ, [Pos; Zero] | EQ, [Zero; Pos] | EQ, [Neg; Zero] | EQ, [Zero; Neg] -> Some Zero
    | (And|Or|Not|LT|EQ), _ -> Some Any
    | _ -> None

  let merge_ty (t1 : ty) (t2 : ty) : ty option =
    if t1 = t2 then Some t1 else Some Any

  let interpret_ty (t : ty) (v : value) : bool =
    match t with
    | Pos -> v > 0 | Zero -> v = 0 | Neg -> v < 0 | Any -> true
end

(* --- Q3: MiniPython --- *)

module MiniPython (S : Theory) = struct
  type expr = | Value of value | Var of string | App of op * expr list
  type cmd = | Assign of string * expr | Ite of expr * cmd * cmd | Seq of cmd * cmd | While of expr * cmd | Skip
  
  let rec eval_expr (m : value StringMap.t) (e: expr) : value = 
    match e with 
    | Value v -> v
    | Var x -> StringMap.find x m
    | App (o, es) -> eval_op o (List.map (eval_expr m) es)

  let rec typecheck_expr (m : S.ty StringMap.t) (e: expr) : S.ty option = 
    match e with
    | Value v -> S.type_of_const v
    | Var x -> StringMap.find_opt x m
    | App (op, args) ->
      let arg_tys_opt = List.map (typecheck_expr m) args in
      if List.exists Option.is_none arg_tys_opt then None
      else S.type_of_op op (List.map Option.get arg_tys_opt)

  let rec eval_cmd (m : value StringMap.t) (c : cmd) : value StringMap.t = 
    match c with 
    | Assign (x, e) -> StringMap.add x (eval_expr m e) m
    | Ite (e, c1, c2) -> if eval_expr m e = 1 then eval_cmd m c1 else eval_cmd m c2
    | Seq (c1, c2) -> eval_cmd (eval_cmd m c1) c2
    | While (e, c) -> if eval_expr m e = 1 then eval_cmd (eval_cmd m c) (While (e, c)) else m
    | Skip -> m

  let rec typecheck_cmd (m : S.ty StringMap.t) (c : cmd) : bool = 
    match c with
    | Assign (x, e) ->
      (match StringMap.find_opt x m, typecheck_expr m e with
       | Some tx, Some te -> tx = te
       | _ -> false)
    | Ite (g, c1, c2) ->
      (match typecheck_expr m g with
       | Some _ -> typecheck_cmd m c1 && typecheck_cmd m c2
       | _ -> false)
    | Seq (c1, c2) -> typecheck_cmd m c1 && typecheck_cmd m c2
    | While (g, body) ->
      (match typecheck_expr m g with
       | Some _ -> typecheck_cmd m body
       | _ -> false)
    | Skip -> true
end