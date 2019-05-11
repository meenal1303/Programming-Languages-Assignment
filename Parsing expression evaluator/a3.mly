%{
    open A1
%}
/*
- Tokens (token name and rules) are modified wrt to A2. Please make necessary changes in A3
- LP and RP are left and right parenthesis
- Write grammar rules to recognize
  - >= <= from GT EQ LT tokens
  - if then else fi
*/
/* Tokens are defined below.  */
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ EOF
%start main
%type <A1.exptree> main /* Return type */
%%
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/
/*InParen > Tuple > Project > IfThenElse > Negative > Abs > (Div = Mult = Rem) > (Add = Sub) > (GreaterT = LessT = GreaterTE = LessTE = Equals) > Not > Conjunction > Disjunction*/
main:
    main_two EOF                            { $1 }
;
main_two:
    disj                                    { $1 }
;  
disj:
    disj DISJ conj                          {Disjunction($1,$3)}
    |conj                                   { $1 }
;
conj:
    conj CONJ not                           {Conjunction($1,$3)}
    |not                                    { $1 }
;
not:
    NOT not                                 {Not($2)}
    |comparison                             { $1 }
;
comparison:
    add_sub_exp EQ add_sub_exp              {Equals($1,$3)}
    |add_sub_exp GT add_sub_exp             {GreaterT($1,$3)}
    |add_sub_exp LT add_sub_exp             {LessT($1,$3)}
    |add_sub_exp GT EQ add_sub_exp          {GreaterTE($1,$4)}
    |add_sub_exp LT EQ add_sub_exp          {LessTE($1,$4)}
    |add_sub_exp                            { $1 }
;
add_sub_exp:
    add_sub_exp MINUS add_sub_exp           {Sub($1,$3)}
    |add_sub_exp PLUS add_sub_exp           {Add($1,$3)}
    |div_mult_rem_exp                       { $1 }
;
div_mult_rem_exp:
    div_mult_rem_exp TIMES div_mult_rem_exp {Mult($1,$3)}
    |div_mult_rem_exp DIV div_mult_rem_exp  {Div($1,$3)}
    |div_mult_rem_exp REM div_mult_rem_exp  {Rem($1,$3)}
    |absolute_neg                           { $1 }
;
absolute_neg:
    ABS absolute_neg                        {Abs($2)}
    |TILDA absolute_neg                     {Negative($2)}
    |ifthenelse                             { $1 }
;
ifthenelse:
    IF main_two THEN main_two ELSE main_two FI          {IfThenElse($2,$4,$6)}
    |project                                            { $1 }
;
project:
    PROJ LP INT COMMA INT RP project        {Project(($3,$5),$7)}
    |tuple                                  { $1 }
;
tuple:
    LP list RP                              { $2 }
    |bracket                                { $1 }
;
list:
    main_two COMMA main_two                     {Tuple(2,[$1]@[$3])}
   | main_two COMMA list                        {(match $3 with
                                                Tuple(i,xs) -> Tuple((i+1),[$1]@xs))}
;
bracket:
    LP main_two RP                          {InParen($2)}
    |constant                               { $1 }
;

constant:
    INT                                     { N($1) }
    |ID                                     { Var($1) }
    |BOOL                                   { B($1) }
;

/*
1.
parser "5+~6" rho ;;
- : A1.exptree * A1.value * A1.answer =
(Add (N 5, Negative (N 6)), NumVal (-1), Num (Neg, [1]))

2.
# parser "if 5>3 then 7 else 9 fi" rho;;
- : A1.exptree * A1.value * A1.answer =
(IfThenElse (GreaterT (N 5, N 3), N 7, N 9), NumVal 7, Num (NonNeg, [7]))

3.
# parser "proj(1,3)(proj(2,2)((5+ 4, if T then 6 else T fi,T),(T /\\ F,4- 8, 6 mod 4)))" rho;;
- : A1.exptree * A1.value * A1.answer =
(Project ((1, 3),
  InParen
   (Project ((2, 2),
     Tuple (2,
      [Tuple (3, [Add (N 5, N 4); IfThenElse (B true, N 6, B true); B true]);
       Tuple (3,
        [Conjunction (B true, B false); Sub (N 4, N 8); Rem (N 6, N 4)])])))),
 BoolVal false, Bool false)

4.
parser "5 - 7 * (8 div 4 mod 6 div 2)" rho ;;
- : A1.exptree * A1.value * A1.answer =
(Sub (N 5, Mult (N 7, InParen (Rem (Div (N 8, N 4), Div (N 6, N 2))))),
 NumVal (-9), Num (Neg, [0; 9]))


*/
