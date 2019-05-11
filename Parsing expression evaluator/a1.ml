(* Dummy implementation of A1 *)
open A0
exception Not_implemented
exception Invalid_tuple_index
exception Stack_Empty
exception DivByZeroException
exception InvalidOperationInGetInt
exception InvalidOperationInGetBool
exception InvalidOperation    (*raised when expression formed with opcode list is not valid i.e. bigint and operators mismatch*)
(*initially bigint list is always kept zero in stackmc*)

let absolute (i:int):int = 
    if i<0 then -(i)
        else i
;;
(*gives starting x elements*)
let rec pop (y:int) b = (match y with
    0->[]
    |_-> (match b with
        []-> raise Invalid_tuple_index
        |x::xs -> (pop (y-1) xs)@[x]))
;;

let rec left (x:int) b = match x with
    0->b
    |y -> (left (y-1) (List.tl(b)))
;;

let rec extract y1 i = match i with
    1->List.hd(y1)
    |_-> (extract (List.tl(y1)) (i-1)) 
;;

(* abstract syntax *)
type  exptree =
  Var of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | N of int      (* Integer constant *)
  | B of bool     (* Boolean constant *)
  (* unary operators on integers *)
  | Abs of exptree                   (* abs *)
  | Negative of exptree              (* unary minus ~ *)
  (* unary operators on booleans *)
  | Not of exptree
  (* binary operators on integers *)
  | Add of exptree * exptree         (* Addition + *)
  | Sub of exptree * exptree         (* Subtraction - *)
  | Mult of exptree * exptree        (* Multiplication * *)
  | Div of exptree * exptree         (* div *)
  | Rem of exptree * exptree         (* mod *)
  (* binary operators on booleans *)
  | Conjunction of exptree * exptree (* conjunction /\ *)
  | Disjunction of exptree * exptree (* binary operators on booleans \/ *)
  (* comparison operations on integers *)
  | Equals of exptree * exptree      (* = *)
  | GreaterTE of exptree * exptree   (* >= *)
  | LessTE of exptree * exptree      (* <= *)
  | GreaterT of exptree * exptree    (* > *)
  | LessT of exptree * exptree       (* < *)
  (* expressions using parenthesis *)
  | InParen of exptree               (* ( ) *)
  (* a conditional expression *)
  | IfThenElse of exptree * exptree * exptree (* if then else fi  *)
  (* creating n-tuples (n >= 0) *)
  | Tuple of int * (exptree list)
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Project of (int*int) * exptree   (* Proj((i,n), e)  0 < i <= n *)
  

(* opcodes of the stack machine (in the same sequence as above) *)
type opcode = VAR of string | NCONST of bigint | BCONST of bool | ABS | UNARYMINUS | NOT | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT | PAREN | IFTE | TUPLE of int | PROJ of int*int;;

(* The type of value returned by the definitional interpreter. *)
type value = NumVal of int | BoolVal of bool | TupVal of int * (value list)

(* The language should contain the following types of expressions:  integers and booleans *)
type answer = Num of bigint | Bool of bool | Tup of int * (answer list) ;;

let rec get_int (k:value) : int = match k with
    NumVal(i) -> i
    |_ ->raise InvalidOperationInGetInt;;

let rec get_bool (k:value) : bool = match k with
    BoolVal(i) -> i
    |_ -> raise InvalidOperationInGetBool;;
    
let rec eval (t:exptree) (rho:(string -> value)) : value = match t with
    N(n)                   -> NumVal(n)
    |B(b)                  -> BoolVal(b)
    |Var(s)                -> (rho s)
    |Add(t1,t2)            -> NumVal((get_int(eval t1 rho)) + (get_int(eval t2 rho)))
    |Sub(t1,t2)            -> NumVal((get_int(eval t1 rho)) - (get_int(eval t2 rho)))
    |Mult(t1,t2)           -> NumVal((get_int(eval t1 rho)) * (get_int(eval t2 rho)))
    |Div(t1,t2)            -> (match get_int(eval t2 rho) with
                                0 -> raise DivByZeroException
                                |_->NumVal((get_int(eval t1 rho)) / (get_int(eval t2 rho))))
    |Rem(t1,t2)            -> NumVal((get_int(eval t1 rho)) mod (get_int(eval t2 rho)))
    |Conjunction(t1,t2)    -> if (get_bool(eval t1 rho) && get_bool(eval t2 rho)) then BoolVal(true) else BoolVal(false)
    |Disjunction(t1,t2)    -> if (get_bool(eval t1 rho) || get_bool(eval t2 rho)) then BoolVal(true) else BoolVal(false)
    |Equals(t1,t2)         -> if ((get_int(eval t1 rho))=(get_int(eval t2 rho))) then BoolVal(true) else BoolVal(false)
    |GreaterTE(t1,t2)      -> if ((get_int(eval t1 rho))>=(get_int(eval t2 rho))) then BoolVal(true) else BoolVal(false)
    |LessTE(t1,t2)         -> if ((get_int(eval t1 rho))<=(get_int(eval t2 rho))) then BoolVal(true) else BoolVal(false)
    |GreaterT(t1,t2)       -> if ((get_int(eval t1 rho))>(get_int(eval t2 rho))) then BoolVal(true) else BoolVal(false)
    |LessT(t1,t2)          -> if ((get_int(eval t1 rho))<(get_int(eval t2 rho))) then BoolVal(true) else BoolVal(false)
    |InParen(t1)           -> (eval t1 rho)
    |IfThenElse(t1,t2,t3)  -> if (get_bool(eval t1 rho)) then (eval t2 rho) else (eval t3 rho)
    |Tuple(a,el)           -> (match el with
                                []->TupVal(a,[])
                                |(x::xs)-> (match (eval (Tuple((a-1),xs)) rho) with
                                                TupVal(t,ys) -> (TupVal(t+1,(eval x rho)::ys))
                                                |_ -> raise InvalidOperation)
                            )
    |Not(t1)               -> BoolVal(not (get_bool(eval t1 rho)))
    |Negative(t1)          -> NumVal( -1*(get_int(eval t1 rho)))
    |Abs(t1)               -> NumVal(absolute(get_int(eval t1 rho)))
    |Project((i,n),t1)     -> (match (eval t1 rho) with 
                                TupVal(n,xs) -> (extract xs i)
                                |_ -> raise InvalidOperation)
;;

(*Postorder*)
let rec compile (t:exptree):opcode list = match t with
    N(n)                   -> [NCONST(mk_big (n))]
    |B(b)                  -> [BCONST(b)]
    |Var(s)                -> [VAR(s)]
    |Add(t1,t2)            -> (compile t2)@(compile t1)@[PLUS]
    |Sub(t1,t2)            -> (compile t2)@(compile t1)@[MINUS]
    |Mult(t1,t2)           -> (compile t2)@(compile t1)@[MULT]
    |Div(t1,t2)            -> (compile t2)@(compile t1)@[DIV]
    |Rem(t1,t2)            -> (compile t2)@(compile t1)@[REM]
    |Conjunction(t1,t2)    -> (compile t2)@(compile t1)@[CONJ]
    |Disjunction(t1,t2)    -> (compile t2)@(compile t1)@[DISJ]
    |Equals(t1,t2)         -> (compile t2)@(compile t1)@[EQS]
    |GreaterTE(t1,t2)      -> (compile t2)@(compile t1)@[GTE]
    |LessTE(t1,t2)         -> (compile t2)@(compile t1)@[LTE]
    |GreaterT(t1,t2)       -> (compile t2)@(compile t1)@[GT]
    |LessT(t1,t2)          -> (compile t2)@(compile t1)@[LT]
    |InParen(t1)           -> (compile t1)@[PAREN]
    |IfThenElse(t1,t2,t3)  -> (compile t1)@(compile t2)@(compile t3)@[IFTE]
    |Tuple(a,el)           -> (match el with
                                []->[TUPLE(a)]
                                |(x::xs)->((compile x)@(compile (Tuple(a,xs)))) )
    |Project((i,n),t1)     -> (compile t1)@[PROJ(i,n)]
    |Abs(t1)               -> (compile t1)@[ABS]
    |Negative(t1)          -> (compile t1)@[UNARYMINUS]
    |Not(t1)               -> (compile t1)@[NOT]
;;
(*val compile : exptree -> opcode list = <fun  (match (compile t1) with
                                x::xs -> [extract xs i]@[PROJ(i,n)]
                                |_ -> raise InvalidOperation)*)



let rec stackmc (b:answer list) (binding:(string -> answer)) (l:opcode list) : answer = (match l with
    []                -> (List.hd b)
    |(NCONST(k)::xs)  -> (stackmc (Num(k)::b) binding xs) 
    |(BCONST(k)::xs)  -> (stackmc (Bool(k)::b) binding xs)
    |(VAR(s)::xs)     -> (stackmc (binding(s)::b) binding xs )
    |(PLUS::xs)       -> ( match b with
                            [] -> raise Stack_Empty
                            |(Num(y1)::(Num(y2)::ys)) -> (stackmc (Num(add y1 y2)::ys) binding xs)
                            |_ -> raise InvalidOperation )
    |(MINUS::xs)      -> ( match b with
                            [] -> raise Stack_Empty
                            |(Num(y1)::(Num(y2)::ys)) -> (stackmc (Num(sub y1 y2)::ys) binding xs)
                            |_ -> raise InvalidOperation )
    |(MULT::xs)       -> ( match b with
                            [] -> raise Stack_Empty
                            |(Num(y1)::(Num(y2)::ys)) -> (stackmc (Num(mult y1 y2)::ys) binding xs)
                            |_ -> raise InvalidOperation )
    |(DIV::xs)        -> ( match b with
                            [] -> raise Stack_Empty
                            |(Num(y1)::(Num(y2)::ys)) -> (stackmc (Num(div y1 y2)::ys) binding xs)
                            |_ -> raise InvalidOperation )
    |(REM::xs)        -> ( match b with
                            [] -> raise Stack_Empty
                            |(Num(y1)::(Num(y2)::ys)) -> (stackmc (Num(rem y1 y2)::ys) binding xs)
                            |_ -> raise InvalidOperation )
    |(ABS::xs)        -> ( match b with
                            [] -> raise Stack_Empty
                            |(Num(y1)::ys) -> (stackmc (Num(abs y1)::ys) binding xs)
                            |_ -> raise InvalidOperation )
    |(UNARYMINUS::xs) -> ( match b with
                            [] -> raise Stack_Empty
                            |(Num(y1)::ys) -> (stackmc (Num(minus y1)::ys) binding xs)
                            |_ -> raise InvalidOperation )
    |(NOT::xs)        -> ( match b with
                            []-> raise Stack_Empty
                            |(y1::ys) -> (if y1 = Bool(true) then (stackmc (Bool(false)::ys) binding xs)
                                            else (stackmc (Bool(true)::ys) binding xs) )
                            )
    |(CONJ::xs)       ->( match b with
                            [] -> raise Stack_Empty
                            |(y1::y2::ys) -> (
                                            if (y1 = Bool(true) && y2 = Bool(true)) then (stackmc (Bool(true)::ys) binding xs)
                                            else (stackmc (Bool(false)::ys) binding xs) )
                            |_ -> raise InvalidOperation)
    |(DISJ::xs)       ->( match b with
                            [] -> raise Stack_Empty
                            |(y1::y2::ys) -> (
                                            if (y1 = Bool(false) && y2 = Bool(false)) then (stackmc (Bool(false)::ys) binding xs)
                                            else (stackmc (Bool(true)::ys) binding xs))
                            |_ -> raise InvalidOperation)
    |(EQS::xs)        ->( match b with
                            [] -> raise Stack_Empty
                            |(Num(y1)::Num(y2)::ys) -> ( if (eq y1 y2) then (stackmc (Bool(true)::ys) binding xs)
                                                                else (stackmc (Bool(false)::ys) binding xs) )
                            |_ -> raise InvalidOperation)
    |(GTE::xs)        ->( match b with
                            [] -> raise Stack_Empty
                            |(Num(y1)::Num(y2)::ys) -> (
                                                            if (geq y1 y2) then (stackmc (Bool(true)::ys) binding xs)
                                                                else (stackmc (Bool(false)::ys) binding xs) )
                            |_ -> raise InvalidOperation)
    |(LTE::xs)        ->( match b with
                            [] -> raise Stack_Empty
                            |(Num(y1)::Num(y2)::ys) -> (
                                                            if (leq y1 y2) then (stackmc (Bool(true)::ys) binding xs)
                                                                else (stackmc (Bool(false)::ys) binding xs) )
                            |_ -> raise InvalidOperation)
    |(GT::xs)         ->( match b with
                            [] -> raise Stack_Empty
                            |(Num(y1)::Num(y2)::ys) -> (
                                                            if (gt y1 y2) then (stackmc (Bool(true)::ys) binding xs)
                                                                else (stackmc (Bool(false)::ys) binding xs) )
                            |_ -> raise InvalidOperation)
    |(LT::xs)         ->( match b with
                            [] -> raise Stack_Empty
                            |(Num(y1)::Num(y2)::ys) -> (
                                                            if (lt y1 y2) then (stackmc (Bool(true)::ys) binding xs)
                                                                else (stackmc (Bool(false)::ys) binding xs) )
                            |_ -> raise InvalidOperation)
    |(PAREN::xs)      -> if (b=[]) then raise Stack_Empty else stackmc b binding xs
    |(IFTE::xs)       ->( match b with
                            [] -> raise Stack_Empty
                            |(y1::y2::Bool(y3)::[]) -> (if y3 then (stackmc [y2] binding xs) else (stackmc [y1] binding xs) )
                            |(y1::y2::Bool(y3)::ys) -> (if y3 then (stackmc (y2::ys) binding xs) else (stackmc (y1::ys) binding xs) )
                            |_ -> raise InvalidOperation
                        )
    |(TUPLE(x)::xs)   -> (stackmc (Tup(x,(pop x b))::(left x b)) binding xs)
    |(PROJ(i,n)::xs)  -> (match b with
				[] -> raise Stack_Empty
				|y::ys -> (match y with 
                                        Tup(n,l) -> (stackmc ([extract l i]@ys) binding xs)
                                        |_ -> raise InvalidOperation))
    |_                -> raise InvalidOperation )
;;
(*(stackmc b binding xs)*)
