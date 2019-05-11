{
  open A3
  exception Not_implemented
}

(*
  Below is a dummy implementation. Please note the following
  - Tokens are defined in A3.mly
  - Return type is token and not token list
  - End of buffer is indicated by EOF token below
  - There is no trailer. The scanner function is written in the wrapper file (test_a4.ml)
  - This is sample implementation. Please rewrite them as per the specifications
*)
(* Creating a lexing rule for the type token defined in parser *)


let whitespace = [' ' '\t' '\n']+
let zero = ['0']+
let digit = ['1'-'9']+
let digits = ['0'-'9']+
let integer = (zero|(digit(digits*)))
let letters = ['a'-'z']+
let letterc = ['A'-'Z']+
let fun = (['a'-'z']|['A'-'Z'])+
let id = letters|digits

rule read = parse
    eof                         {EOF}
  | integer as i                {INT (int_of_string i)}
  | whitespace                  {read lexbuf}  (* skip whitespace *)
  | "("                         {LP} 
  | ")"                         {RP}
  | ":"                         {COLON}
  | "="                         {EQUALS}
  | ","                         {COMMA}
  | "call"                      {CALL}
  | "ret"                       {RETURNS}
  | letters as l                {VAR l}
  | fun as l                    {FNAME l}
  | _                           {raise Not_implemented }
