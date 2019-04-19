open Printf

(* Add *)
type symbol = Cross | Knot | Null

(* Add *)
type t = {game_board : symbol list}

(* Add *)
type result = Legal of t | Illegal 

(* Add *)
let init = {game_board = 
              [Null; Null; Null; Null; Null; Null; Null; Null; Null;]}

(* Add *)
exception IndexOutOfBounds

(* Add *)
let rec make_move (sym:symbol) (idx:int) (counter:int)
    (acc:symbol list) (board:symbol list) = 
  match board with
  | [] -> Illegal
  | x::xs when idx = counter -> Legal {game_board = acc@[sym]@xs}
  | x::xs -> make_move (sym) (idx) (counter+1) (acc@[x]) (xs)


let check_over (board:symbol list) : bool = match board with
  | [Cross; Cross; Cross; _; _; _; _; _; _] -> true
  | [Knot; Knot; Knot; _; _; _; _; _; _] -> true
  | [ _; _; _; Cross; Cross; Cross; _; _; _] -> true
  | [ _; _; _; Knot; Knot; Knot; _; _; _] -> true
  | [ _; _; _; _; _; _; Cross; Cross; Cross] -> true
  | [ _; _; _; _; _; _ ; Knot; Knot; Knot] -> true
  | [ Cross; _; _; _; Cross; _; _; _; Cross] -> true
  | [ Knot; _; _; _; Knot; _; _; _; Knot] -> true
  | [ _; _; Cross; _; Cross; _; Cross; _; _] -> true
  | [ _; _; Knot; _; Knot; _; Knot; _; _] -> true
  | _ -> false

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

let transform coordinate : int = 
  match coordinate with
  | (1, 1) -> 6
  | (1, 2) -> 3
  | (1, 3) -> 0
  | (2, 1) -> 7
  | (2, 2) -> 4
  | (2, 3) -> 2
  | (3, 1) -> 8
  | (3, 2) -> 5
  | (3, 3) -> 2
  | _ -> 100

let get_char sym = match sym with
  | Cross -> 'X'
  | Knot -> 'O'
  | Null -> ' '

let print_game st =
  begin
    let horizontal_div = "   -----------" in
    let board = st.game_board in
    let nth n = List.nth board n in
    print_newline ();
    printf "3   %c | %c | %c " (get_char (nth 0)) 
      (get_char (nth 1)) (get_char (nth 2));
    print_newline ();
    print_string horizontal_div;
    print_newline ();
    printf "2   %c | %c | %c " (get_char (nth 3)) 
      (get_char (nth 4)) (get_char (nth 5));
    print_newline ();
    print_string horizontal_div;
    print_newline ();
    printf "1   %c | %c | %c " (get_char (nth 6)) 
      (get_char (nth 7)) (get_char (nth 8));
    print_newline ();
    print_newline ();
    print_string "    1   2   3";
    print_newline ();
  end

let rec process move_num st : t = 
  print_endline "";
  print_game st;
  print_endline "Please enter a command.";
  print_string "> ";
  let input = read_line () in 
  match parse input with
  | exception Malformed ->
    print_endline "Your command is malformed. Try again." ;
    process move_num st
  | exception Empty ->
    print_endline "Please enter something besides whitespace.";
    process move_num st
  | Quit ->
    print_endline "\nGoodbye";
    exit 0
  | Mark (a, b) -> 
    let pos = transform (int_of_string a, int_of_string b) in
    if pos = 100 then begin
      print_endline  "Invalid input location.";
      process move_num st
    end
    else
      match List.nth st.game_board pos with
      | Cross -> print_endline "There is already a cross here!" ;
        process move_num st
      | Knot -> print_endline "There is already a knot here!";
        process move_num st
      | Null -> 
        let sym = if move_num mod 2 = 0 then Cross else Knot in
        match make_move sym pos 0 [] st.game_board with
        | Illegal -> 
          print_endline "Invalid move, try again.";
          process move_num st
        | Legal(new_state) -> 
          if move_num mod 2 = 0 then
            if check_over st.game_board
            then begin print_endline "Game over! Knots Win!"; exit 0 end 
            else
            if check_over st.game_board
            then begin print_endline "Game over! Crosses Win"; exit 0 end;
          process (move_num + 1) (new_state)

(** [play_game f] starts the adventure in file [f]. *)
let play_game () move_num =
  process move_num init

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to OTicTacToe.\n");
  print_endline "Press enter to begin!\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "quit" -> print_endline ("Adios!"); exit 0;
  | _ -> play_game () 0

(* Execute the game engine. *)
let () = main ()