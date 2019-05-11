%{
    open A6
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
%token <string> VAR
%token <string> FNAME
%token LP RP COLON EQUALS COMMA CALL RETURNS FNAME EOF

%start exp_parser 
%type <A6.exptree> exp_parser /* Returns expression */
%%
/* The grammars written below are dummy. Please rewrite it as per the specifications. */

/* Implement the grammar rules for expressions, which may use the parser for definitions */
exp_parser:
    calling EOF                                 { $1 }
;
calling:
    CALL FNAME LP argument RP                   {Call($2,$4) }
    |constant                                   { $1 }
;
argument:
    constant COMMA constant                     {F($1,$3)} 
;
constant:
    VAR COLON EQUALS constant                   {Value(Var($1),$4)}
    |INT                                        {N($1)}
    |VAR                                        {Var($1)}
    |RETURNS                                    {Ret }
;

