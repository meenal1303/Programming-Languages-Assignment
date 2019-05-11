type expr = 
    V of string 
    | Bool of bool 
    | Integer of int 
    | Cmp of expr 
    | Lambda of (expr * expr) 
    | App of (expr * expr) 
    | Plus of (expr * expr) 
    | Mult of (expr * expr) 
    | And of (expr * expr) 
    | Or of (expr * expr) 
    | If_Then_Else of (expr * expr * expr)
    
type types = Tint | Tbool | Tfunc of types*types

val type_check : expr->types list->types
val type_call : expr->bool

type k_answer = 
     Num of int*((expr*k_answer) list)
    |Boolean of bool*((expr*k_answer) list)
    |Var of string*((expr*k_answer) list)
    |Clos of (expr * ((expr*k_answer) list))

val check : (k_answer list)->((expr*k_answer) list)->string->k_answer
val krivine : expr->k_answer list->(expr*k_answer) list->k_answer

type opcode = VAR of string | B of bool | N of int | PLUS | MULT | CONJ | DISJ | CMP | COND of ((opcode list)*(opcode list)) | CLOS of ((opcode list)*(opcode list)) | RET | APP
type s_answer = Nval of int | Bval of bool | Closure of ((string) * (opcode list) * ((string*s_answer) list) )
type dump = (s_answer list) * ((string*s_answer) list) * (opcode list) ;;

val compile : expr->opcode list
val find : ((string*s_answer) list)->string->s_answer
val secd : opcode list->s_answer list->((string*s_answer) list)->dump list->s_answer

