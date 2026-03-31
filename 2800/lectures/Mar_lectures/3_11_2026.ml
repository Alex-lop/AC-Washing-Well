module MachineTrace (M : StateMachine) = struct
  type trace = (M.state * M.input * M.state) list

  let run_machine (inputs : M.input list) : trace option = 
    let rec go state inputs = 
       match inputs with 
       | [] -> []
       | i :: inputs' -> 
          let state' = M.step state i in 
          (state, i, state') :: go state' inputs'
    in 

    try 
      (* Your first try-catch block in OCaml *)
      Some (go M.init inputs)
    with Failure _ -> None
end;;

module MkTrace = MachineTrace(MoneyItemVendingMachine)

open MoneyItemVendingMachine

let gen_input () : input =
  match Random.int 3 with
  | 0 -> Insert25c
  | 1 -> Insert50c
  | 2 -> Vend
  | _ -> failwith "invalid integer"

let gen_trace n () : Trace.trace option = 
  let is = List.init (Random.int n) (fun _ -> gen_input ()) in 
  Trace.run_machine is


let rec length (lst : 'a list) : int =
match lst with 
| [] -> 0
| h :: t -> 1 + length t

let rec append lst1 lst 2 =
  match lst1 with 
  | [] -> lst2
  | h :: t -> h :: append t lst2

  