module type StateMachine = sig
  type state
  type input
  val init : state
  val step : state -> input -> state
end

module MoneyItemVendingMachine (* : StateMachine *) = struct
  type state = {money : float; items: int} (* money should not be float bc can't have 0.0001 and items should not be int bc it can be negative*)
  type input = Insert25c | Insert50c | Vend

  let init = { money = 0.0 ; items = 10}

  let step state input =
    match input with
    | Insert25c ->
        { state with money = state.money +. 0.25 }
        
    | Insert50c -> { state with money = state.money +. 0.50 }
    | Vend when state.money >= 1.00 && state.items >= 1 ->
      { money = state.money -. 1.00; items = state.items - 1}

    | Vend when state.money < 1.00 -> failwith "not enough money"
    | Vend when state.items < 1 -> failwith "not enoguht items"
    | Vend -> failwith "invalid state"
      
end

    (* let step state input =
    match input with
    | Insert25c -> 
        { state with money = state.money +. 0.25 }
        
    | Insert50c -> 
        { state with money = state.money +. 0.50 }
        
    | Vend -> 
        (* We check if items > 0 to prevent a negative count *)
        if state.items > 0 then
          { money = 0.0; items = state.items - 1 }
        else
          state *)