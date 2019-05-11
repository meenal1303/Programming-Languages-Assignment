
exception Error;;

(* abstract syntax *)
type  exptree =
  Var of string
  | N of int      
  | Value of exptree * exptree
  | F of exptree * exptree
  | Call of string * exptree                  
  | Ret
;;

val cs : (string * string list * string list * ( ( string * (int ref) ) list) * ( ( string * ( int ref ) ) list ) ) list ref;; (*dynamic links*)
val node : string -> int -> int -> (string * string list * string list * ( ( string * (int ref) ) list) * ( ( string * ( int ref ) ) list ) )
val sl : string list ref;; (*static links*)
val cf : string list ref;; (*What procedures can be called*)
val va : (string * int ref) list ref;; 
val tree_info : string ->string list * string list * (string * int ref) list *(string * int ref) list;;
val get_var_acc : string list -> (string * string list * string list * ( ( string * (int ref) ) list) * ( ( string * ( int ref ) ) list ) ) list->(string * int ref) list 
val g_par : string -> string list -> string list
val get_static_link : 'a * 'b * 'c * 'd * 'e -> 'b 
val get_childs : string list -> string list 
val get_frame : 'a list -> string list -> string list 
val rep2 : (string * int ref) list -> string -> int -> unit 
val contains : 'a -> ('a * 'b) list -> bool 
val replace :
  'a list ->
  ('a * 'b * 'c * 'd * (string * int ref) list) list -> string -> int -> unit 
val eval : exptree -> unit
