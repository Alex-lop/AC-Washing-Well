(* If you would like, write how many hours you spent on this homework:

  4

*)

(** HELPER FUNCTIONS **)

let string_of_char_list (cs : char list) : string = 
    String.of_seq (List.to_seq cs)

let char_list_of_string (s : string) : char list = 
    List.of_seq (String.to_seq s)


let last (xs : 'a list) : 'a option = 
    if List.is_empty xs then None else Some (List.nth xs (List.length xs - 1))


(* General state machine type *)

module type StateMachine = sig
  type state
  type input
  val init : unit -> state
  val step : state -> input -> state
end

(* CSV format types *)

type token = | TokChar of char  (* Any character except `,`, `\n`, and `^` *)
             | TokEscaped of string (* string cannot contain character ^ *)
type value = token list (* Can be arbitrarily long *)
type row = value list (* must have length n, where n is number of columns *)
type csv = row list (* Only used if we are talking about the ENTIRE CSV (not when we make the state machine) *)

(* Helper functions to convert CSV data back to strings *)
let string_of_token (t : token) = 
  match t with 
  | TokChar c -> String.init 1 (fun _ -> c)
  | TokEscaped s -> "^" ^ s ^ "^"
let string_of_value (v : value) = String.concat "" (List.map string_of_token v)
let string_of_row (r : row)     = String.concat "," (List.map string_of_value r)
let string_of_csv (c : csv)     = String.concat "\n" (List.map string_of_row c)


(** Q1 **)
(* OK so in this function we need to check 2 things:
1. The number of columns in each row must be the same,
2. Each value has well formed tokens,
 - > And a well formed token means that: TokChar can't contain \n, ^, or ,  AND      TokEscaped can't contain ^ *)
let well_formed_csv (n : int) (c : csv) : bool =

  let well_formed_token (t : token) : bool = 
    match t with
    | TokChar c -> c <> '\n' && c <> '^' && c <> ','
    | TokEscaped s -> 
      let char_of_TokEscaped = char_list_of_string(s) in
      not (List.exists (fun c -> c = '^') char_of_TokEscaped) (* Bscilly checking if in this new list of characters there is a '^', and if true the 'not' make it false *)
  in

  let well_formed_value (v : value) : bool = 
    List.for_all well_formed_token v
  in 

  let all_rows_are_uniform (r : row) : bool = 
    List.length r = n && List.for_all well_formed_value r
  in

  List.for_all all_rows_are_uniform c
    (*OK so the first funcition checks if the token is well formed, then the second fxn tests if the values have all (FORALL) valid tokens,
      then the third fxn firstly checks if the number of columns is the same as the number of rows, and THEN tests if all the values are vaild
      (this will all be ran in the final function that checks if the entire csv is well formed), then the LAST fxn tests if all the rows are uniform
      and if all the values are well formed *)

(** Q2 **)
type csv_state = {
  acc_csv : row list;           (* Accumulated values for the CSV *)
  ncols : int option;           (* Number of columns in a row (must be inferred) *)
  acc_value : token list;       (* Characters accumulated for the current value *)
  acc_row : value list;         (* Accumulated values for the current row *)
  in_escape : bool;             (* Are we inside an escape (^) section? *)
  acc_escaped : char list;      (* Characters accumulated for current TokEscaped, only if in_escape = true *)
  vals_in_row : int;            (* Number of values parsed so far in current row *)
  error : bool                  (* Has an error occurred? If true, parser will stay in error state *)
}

let string_of_option f = function
  | None   -> "None"
  | Some x -> "Some " ^ f x

(* feel free to modify the string to fit your needs *)
let string_of_state (s : csv_state) : string =
  Printf.sprintf
    "{ncols=%s; vals_in_row=%d; in_escape=%b; error=%b; acc_value=%s; acc_row=%s; acc_csv=%s; acc_escaped=%S}"
    (string_of_option string_of_int s.ncols)
    s.vals_in_row
    s.in_escape
    s.error
    (string_of_value s.acc_value)
    (string_of_row s.acc_row)
    (string_of_csv s.acc_csv)
    (string_of_char_list s.acc_escaped)

module CSVStateMachine = struct
  type state = csv_state
  type input = char

  let init () = {
    acc_csv = [];
    ncols = None;
    acc_value = [];
    acc_row = [];
    in_escape = false;
    acc_escaped = [];
    vals_in_row = 0; (* Fixed typo from val_in_row *)
    error = false;
  }
  (* Ok so basically just need to follow the requirments they gave us to keep track of step states*)
  let step (s : state) (i : input) : state =
    if s.error then s 
    else 
      if s.in_escape then
        match i with
        | '^' -> (* Changed from "^" string to '^' char *)
            (* Action: Finish the escaped string and package it as a token *)
            let escaped_str_value = string_of_char_list (List.rev s.acc_escaped) in
            let finished_token = TokEscaped escaped_str_value in
            
            { s with in_escape = false; 
                     acc_value = s.acc_value @ [finished_token]; 
                     acc_escaped = [] }

        | _ -> 
            (* Action: Collect characters into the escaped list (reversed for speed) *)
            { s with acc_escaped = i :: s.acc_escaped }

      else (* Non-escape mode *)
        match i with
        | '^' -> 
            { s with in_escape = true; acc_escaped = [] }
            
        | ',' -> 
            (* Action: Move the finished token list (value) into the current row *)
            let updated_row = s.acc_row @ [s.acc_value] in
            { s with acc_row = updated_row;
                     acc_value = [];
                     vals_in_row = s.vals_in_row + 1 }

        | '\n' -> 
            (* Action: Wrap up the final value and the full row *)
            let final_value = s.acc_value in
            let complete_row = s.acc_row @ [final_value] in
            let column_count = s.vals_in_row + 1 in
            
            (* Logic: Ensure this row matches the established column width *)
            let is_first_row = Option.is_none s.ncols in
            let width_matches = match s.ncols with
              | Some n -> column_count = n
              | None -> true 
            in
            
            if not width_matches then { s with error = true }
            else 
              { s with acc_csv = s.acc_csv @ [complete_row];
                       ncols = (if is_first_row then Some column_count else s.ncols);
                       acc_row = [];
                       acc_value = [];
                       vals_in_row = 0 }

        | _ -> 
            (* Action: Standard character becomes a TokChar in the current value *)
            let new_token = TokChar i in
            { s with acc_value = s.acc_value @ [new_token] }
end

module MachineTrace (M : StateMachine) = struct
  type trace = (M.state * M.input * M.state) list

  let run_machine (inputs : M.input list) : trace option = 
    let rec go state inputs = 
       match inputs with 
       | [] -> []
       | i :: inputs' -> 
          let state' = M.step state i in 
          let o = (state, i, state') in
          o :: go state' inputs'
    in 
    try 
      Some (go (M.init ()) inputs)
    with Failure _ -> None
end;;

module CSV = MachineTrace(CSVStateMachine)

let unescape_newlines (s : string) : string =
  let rec aux acc chars =
    match chars with
    | '\\' :: 'n' :: rest -> aux ('\n' :: acc) rest
    | c :: rest           -> aux (c :: acc) rest
    | []                  -> List.rev acc
  in
  s
  |> String.to_seq
  |> List.of_seq
  |> aux []
  |> List.to_seq
  |> String.of_seq

let run_with_string (s : string) : unit =
  let open CSVStateMachine in
  print_endline ("INIT: " ^ string_of_state (init ()));
  let inp = unescape_newlines s in
  let chars = char_list_of_string inp in
  let rec loop (st : state) (cs : input list) =
    match cs with
    | [] -> ()
    | c :: cs' ->
        let st' = step st c in
        let showc = if c = '\n' then "\\n" else String.make 1 c in
        print_endline ("PARSE[" ^ showc ^ "]: " ^ string_of_state st');
        loop st' cs'
  in
  loop (init ()) chars

let run_with_input () : unit =
  print_string "Enter a CSV line (no final newline needed): ";
  flush stdout;
  let line = read_line () in
  (* We need to add the newline so a row will finish processing *)
  run_with_string (line ^ "\n")


(** Q3 **)
let last (xs : 'a list) : 'a option =
  if List.is_empty xs then None 
  else Some (List.nth xs (List.length xs - 1))

(* Helper function to run a state machine on a list of characters. *)
(* To not miss out on any data, we assume here that `cs` must end with a newline. *)
let make_parsed_csv (cs : char list) : csv option =
  match CSV.run_machine cs with
  | None -> None
  | Some xs ->
    (match last xs with
    | None -> None
    | Some (_, _, s) -> Some s.acc_csv)

(* Serializer for CSV. *)
let serialize_csv (c : csv) =
  char_list_of_string (string_of_csv c) @ ['\n']

(* Parsing, then serializing, gets you back to where you started. *)
let parse_serialize_prop (cs : char list) =
  if last cs = Some '\n'
  then
    match make_parsed_csv cs with
    | None -> true
    | Some parsed ->
      let cs' = serialize_csv parsed in
      cs = cs'
    else true

(* Serializing, then parsing, gets you back to where you started. *)
let serialize_parse_prop (n : int) (c : csv) : bool =
  if well_formed_csv n c
  then make_parsed_csv (serialize_csv c) = Some c
  else true

let rec forall ?(n : int = 1000) (gen : unit -> 'a) (prop : 'a -> bool) =
  if n <= 0 then None else
    let x = gen () in
    if prop x then forall ~n:(n - 1) gen prop else Some x



(* Helper: Generate a random character that isn't a control/special char *)
let gen_random_char () =
  let pool = "abcdefghijklmnopqrstuvwxyz0123456789 ABCDE!" in
  pool.[Random.int (String.length pool)]

(* Property 1:

forall nrows >= 2, forall cs : char list,
    parse_serialize_prop cs = true

*)

let check_parse_serialize () : (int * char list) option =
  forall
    (fun () -> 
       (* Generator for (int * char list) *)
       let n = 2 + Random.int 3 in
       let make_val () = List.init (Random.int 5) (fun _ -> gen_random_char ()) in
       let make_row () = 
         let values = List.init n (fun _ -> make_val ()) in
         List.flatten (List.mapi (fun i v -> if i < n - 1 then v @ [','] else v) values)
       in
       let num_rows = 1 + Random.int 3 in
       (* Making sure too add '\n' to the end of the list to ensure the row is finished *)
       let chars = List.flatten (List.init num_rows (fun _ -> make_row () @ ['\n'])) in
       (n, chars))
    (fun (n, cs) -> parse_serialize_prop cs = true)


(* Property 2:

forall nrows >= 2, forall c : csv,
    serialize_parse_prop nrows c = true.

*)

let check_serialize_parse () : (int * csv) option =
  forall
    (fun () -> 
       (* Generator for (int * csv) *)
       let n = 2 + Random.int 3 in
       let gen_token () = 
         if Random.bool () then TokChar (gen_random_char ())
         else TokEscaped "escaped,content\n" 
       in
       let gen_val () = List.init (Random.int 4) (fun _ -> gen_token ()) in
       let gen_row () = List.init n (fun _ -> gen_val ()) in
       let num_rows = 1 + Random.int 3 in
       (n, List.init num_rows (fun _ -> gen_row ())))
    (fun (n, c) -> serialize_parse_prop n c = true)


(* TODO:
  1. Design generators for number of columns, `char list` and `csv` to test the validity of the two properties above.
  2. Test both propertis using your generators, and, if you faced any counter-examples, explain why they occured. 
  3. Write up your results in a report below in a comment, including an argument for why the generators you wrote were good choices.

  1 (answers part 3 as well): Gen Design: I used 'forall' with two generators: one for random 
      (int * char list) ending in '\n' (Prop 1) and one for well-formed OCaml 
      'csv' types (Prop 2). These are a good fit as they stress-test 
      edge cases like empty values (,,) and escaped newlines (^...^).
      Used random.int to generate the number of columns and rows, and 
      then used List.init to generate the values and rows. Important note: I also created a
      helper function to generate a random character that isn't a control/special char.

   2. Test Results: Property 2 (Serialize -> Parse) returned TRUE for 1000 
      tests. Property 1 (Parse -> Serialize) produced counter-examples as 
      noted in the Piazza update. The results show that while our machine 
      reconstructs data perfectly, it does not always reconstruct the 
      exact original string formatting.

   3. COUNTER EXAMPLES!!: Property 1 fails because the parser is a bit too "lossy" with 
      formatting. While the data remains identical, the serialized string 
      may lack the original's redundant carats or specific whitespace, 
      causing the character-for-character comparison (cs = cs') to fail.

*)
