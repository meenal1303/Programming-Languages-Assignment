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
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ LET IN END BACKSLASH DOT DEF SEMICOLON PARALLEL LOCAL TINT TUNIT TBOOL COLON EOF
%start def_parser exp_parser type_parser
%type <A1.definition> def_parser /* Returns definitions */
%type <A1.exptree> exp_parser /* Returns expression */
%type <A1.exptype> type_parser /* Returns expression */
%%
/* The grammars written below are dummy. Please rewrite it as per the specifications. */

/* Implement the grammar rules for expressions, which may use the parser for definitions */
exp_parser:
    function_call EOF                                                  { $1 }
;
function_call:
    function_call LP function_call RP                                  {FunctionCall($1,$3)}
    |function_abs                                                      { $1 }
;
function_abs:
    BACKSLASH ID COLON type_parser DOT function_call                   {FunctionAbstraction($2,$4,$6)}
    |let_function                                                      { $1 }
;
let_function:
    LET definitions IN function_call END                               {Let($2,$4)}
    |main_two                                                          { $1 }
; 
main_two:
    disj                                                               { $1 }
;
disj:
    disj DISJ conj                                                     {Disjunction($1,$3)}
    |conj                                                              { $1 }
;
conj:
    conj CONJ not                                                      {Conjunction($1,$3)}
    |not                                                               { $1 }
;
not:
    NOT not                                                            {Not($2)}
    |comparison                                                        { $1 }
;
comparison:
    add_sub_exp EQ add_sub_exp                                         {Equals($1,$3)}
    |add_sub_exp GT add_sub_exp                                        {GreaterT($1,$3)}
    |add_sub_exp LT add_sub_exp                                        {LessT($1,$3)}
    |add_sub_exp GT EQ add_sub_exp                                     {GreaterTE($1,$4)}
    |add_sub_exp LT EQ add_sub_exp                                     {LessTE($1,$4)}
    |add_sub_exp                                                       { $1 }
;
add_sub_exp:
    div_mult_rem_exp MINUS div_mult_rem_exp                            {Sub($1,$3)}
    |div_mult_rem_exp PLUS div_mult_rem_exp                            {Add($1,$3)}
    |div_mult_rem_exp                                                  { $1 }
;
div_mult_rem_exp:
    absolute_neg TIMES absolute_neg                                    {Mult($1,$3)}
    |absolute_neg DIV absolute_neg                                     {Div($1,$3)}
    |absolute_neg REM absolute_neg                                     {Rem($1,$3)}
    |absolute_neg                                                      { $1 }
;
absolute_neg:
    ABS absolute_neg                                                   {Abs($2)}
    |TILDA absolute_neg                                                {Negative($2)}
    |ifthenelse                                                        { $1 }
;
ifthenelse:
    IF function_call THEN function_call ELSE function_call FI          {IfThenElse($2,$4,$6)}
    |project                                                           { $1 }
;
project:
    PROJ LP INT COMMA INT RP project                                   {Project(($3,$5),$7)}
    |tuple                                                             { $1 }
;
tuple:
    LP list RP                                                         { $2 }
    |bracket                                                           { $1 }
; 
list:
    function_call COMMA function_call                                  {Tuple(2,[$1]@[$3])}
   |function_call COMMA list                                           {(match $3 with
                                                                              Tuple(i,xs) -> Tuple((i+1),[$1]@xs))
                                                                       }
;
bracket: 
    LP function_call RP                                                {InParen($2)}
    |constant                                                          { $1 }
;

constant:
    INT                                                                { N($1) }
    |ID                                                                { Var($1) }
    |BOOL                                                              { B($1) }
;

/* Implement the grammar rules for definitions, which may use the parser for expression  */
def_parser:
    definitions EOF                                                    { $1 }
;
definitions:
    definitions SEMICOLON basic_def                                    {Sequence(($1)::[$3])}
    |definitions PARALLEL basic_def                                    {Parallel(($1)::[$3])}
    |basic_def                                                         { $1 }
;
basic_def: 
    DEF ID COLON type_parser EQ function_call                          {Simple($2,$4,$6)}
    |LOCAL definitions IN definitions END                              {Local($2,$4)}
;
type_parser:
    LP type_parser RP                                                  { $2 }
    |LP type_exp_list RP                                               { Ttuple($2) } 	
    |type_parser MINUS GT type_parser                                  { Tfunc($1,$4) }
    |TINT                                                              { Tint }
    |TUNIT                                                             { Tunit }
    |TBOOL                                                             { Tbool }
;
type_exp_list:
    type_parser COMMA type_exp_list                                    {(match $3 with
                                                                            (xs) ->([$1]@xs))   
                                                                       }
    |type_parser TIMES type_exp_list                                   {(match $3 with
                                                                            (xs) ->([$1]@xs))   
                                                                       }
                                                                
    |type_parser                                                       { [$1] }
;
    
