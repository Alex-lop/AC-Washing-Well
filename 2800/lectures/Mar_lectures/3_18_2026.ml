let wont_crash (f : () => 'a) : bool =
  try
    let _ = f () in true
    with | _ -> false

let interpre_ty ty : value -> bool ==
    match ty with
        | Tnat -> (fxn | Nat _ -> true | _ -> false)
        | TBool -> (fxn | Bool _ -> true | _ -> false)

let prop_typesound (e : expr) : bool =
match typeof StringMap.empty e with
| None -> true
| Some ty -> wont_crash (fun () -> eval StringMap.empty e) && interpret ty t (eval StringMap.empty e)