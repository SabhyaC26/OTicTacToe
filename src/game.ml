open Printf

(* Add *)
type symbol = Cross | Knot | Null

(* Add *)
type t = {game_board : symbol list; move_num : int}

(* Add *)
type result = Legal of t | Illegal 

(* Add *)
let init = {game_board = 
              [Null; Null; Null; Null; Null; Null; Null; Null; Null;]; 
            move_num = 0}

(* Add *)
exception IndexOutOfBounds

(* Add *)
let rec make_move (sym:symbol) (idx:int) (counter:int)
    (acc:symbol list) (board:symbol list) (st:t) = 
  match board with
  | [] -> Illegal
  | x::xs when idx = counter -> Legal {game_board = acc@[sym]@xs; 
                                       move_num = st.move_num + 1}
  | x::xs -> make_move (sym) (idx) (counter+1) (acc@[x]) (xs) (st)

(* Add *)
type command =
  | Mark of string * string
  | Quit

(* Add *)
exception Malformed

(* Add *)
exception Empty

(* Add *)
let parse str = 

  (* Add *)
  let make_quit (obj : string list) : command = 
    match obj with
    | [] -> Quit
    | h::t -> raise Malformed in

  (* Add *)
  let rec make_mark (obj : string list) : command = 
    if List.length obj = 2
    then
      match obj with
      | a::b::[] -> Mark (a, b)
      | _ -> raise Malformed
    else raise Malformed in 

  (* Add *)
  let check_first (verb : string) (obj : string list) : command = 
    match verb with
    | "mark" -> make_mark obj
    | "quit" -> make_quit obj
    | _ -> raise Malformed in

  (* Add *)
  let check_valid (word_list : string list) : command = 
    match word_list with
    | [] -> raise Empty
    | h::t -> check_first h t in

  str |> 
  String.split_on_char ' ' |> 
  List.filter (fun s -> s <> "") |>
  check_valid

let get_char sym = match sym with
  | Cross -> 'X'
  | Knot -> 'O'
  | Null -> ' '

let print_game st =
  let horizontal_div = "-----------" in
  let board = st.game_board in
  let nth n = List.nth board n in
  print_newline ();
  printf " %c | %c | %c " (get_char (nth 0)) 
    (get_char (nth 1)) (get_char (nth 2));
  print_newline ();
  print_string horizontal_div;
  print_newline ();
  printf " %c | %c | %c " (get_char (nth 3)) 
    (get_char (nth 4)) (get_char (nth 5));
  print_newline ();
  print_string horizontal_div;
  print_newline ();
  printf " %c | %c | %c " (get_char (nth 6)) 
    (get_char (nth 7)) (get_char (nth 8));
  print_newline ();
  print_newline ();