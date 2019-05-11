%{
    open A0
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
%token PLUS TIMES CONJ DISJ LP RP IF THEN ELSE FI BACKSLASH DOT CMP EOF 
%start exp_parser
%type <A0.expr> exp_parser /* Returns expression */
%%

exp_parser:
    main2 EOF						{ $1 }
;
main2:
    disj                                                { $1 }
;
disj:
    disj DISJ conj					{Or($1,$3)}
    |conj						{ $1 }
;
conj:
    conj CONJ cmp					{And($1,$3)}
    |cmp						{ $1 }
;	
cmp:
    CMP LP main2 RP					{Cmp($3)}
    |add						{ $1 }
;
add:
    add PLUS mult					{Plus($1,$3)}
    |mult						{ $1 }
;
mult:
    ifthenelse TIMES mult				{Mult($1,$3)}
    |ifthenelse						{ $1 }
;
ifthenelse:
    IF main2 THEN main2 ELSE main2 FI                   {If_Then_Else($2,$4,$6)}
    |function_call                                      { $1 }
;
function_call:
    function_call LP main2 RP                           {App($1,$3)}
    |function_abs                                       { $1 }
;
function_abs:
    BACKSLASH constant DOT function_abs                 {Lambda($2,$4)}
    |bracket                                            { $1 }
;
bracket: 
    LP main2 RP                                         { $2 }
    |constant                                           { $1 }
;
constant:
    ID							{V($1)}
    |INT						{Integer($1)}
    |BOOL						{Bool($1)}
; 



