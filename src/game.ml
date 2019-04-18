(* Add *)
type symbol = Cross | Knot | Null

(* Add *)
type t = {game_board : symbol list; move_num : int}

(* Add *)
type result = Legal of t | Illegal 

(* Add *)
let initialize = {game_board = 
                    [Null; Null; Null; Null; Null; Null; Null; Null; Null;]; 
                  move_num = 0}

(* Add *)
exception IndexOutOfBounds

(* Add *)
let rec make_move (sym:symbol) (idx:int) (counter:int) 
    (acc:symbol list) (st:t) = 
  match st.game_board with
  | [] -> Illegal
  | h::t when idx = counter -> Legal {game_board = acc@[sym]@t; 
                                      move_num = st.move_num + 1}
  | h::t -> make_move (sym) (idx) (counter+1) (acc@[h]) (st)
