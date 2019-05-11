{
  open A2
  exception Not_implemented
}

(*
  Below is a dummy implementation. Please note the following
  - Tokens are defined in A2.mly
  - Return type is token and not token list
  - End of buffer is indicated by EOF token below
  - There is no trailer. The scanner function is written in the wrapper file (test_a4.ml)
  - This is sample implementation. Please rewrite them as per the specifications
*)
let whitespace = [' ' '\t']+
let zero = ['0']
let digit = ['1'-'9']
let digits = ['0'-'9']
let integer = ('-')?(zero|(digit(digits*)))
let int2 = ('+'|'-')?(zero(digit+))
let letters = ['a'-'z']
let letterc = ['A'-'Z']
let id = (letterc)(letterc|letters|digits|'_'| '\'' )*
let binary_boolean_and = "/\\"
let binary_boolean_or = "\\/"
let backslash = "\\"

rule read = parse
    eof                         {EOF}
   |integer as i                {INT (int_of_string i)}
   |"("                         {LP} 
   |")"                         {RP}
   |"+"                         {PLUS}
   |"*"                         {TIMES}
   |"if"                        {IF}
   |"then"                      {THEN}
   |"else"                      {ELSE}
   |"fi"                        {FI}
   |"."                         {DOT}
   |backslash                   {BACKSLASH}
   |"cmp"                       {CMP}
   |"T"                         {BOOL true}
   |"F"                         {BOOL false}
   |binary_boolean_and          {CONJ}
   |binary_boolean_or           {DISJ}
   |id as i                     {ID i}
   |whitespace                  {read lexbuf}
   |int2 as k                   {failwith ("INVALID_INPUT ,CANNOT START FROM "^(Char.escaped k.[0]))}
   |_ as p                      {failwith ("INVALID_INPUT "^(Char.escaped p))}
   | _                          { raise Not_implemented }
