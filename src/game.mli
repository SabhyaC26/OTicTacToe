type symbol = Cross | Knot | Null

type t = {game_board : symbol list; move_num : int}

type result = Legal of t | Illegal 

exception IndexOutOfBounds

type command =
  | Mark of string * string
  | Quit

exception Malformed

exception Empty
