%{
%}

%token <float> NUMBER
%token <string> SYMBOL
%token <string> STRING
%token TRUE FALSE
%token IF
%token LPAREN RPAREN
%token DOT
%token EOF

%start expr
%type <Type.expr> expr

%%
expr:
    NUMBER { Type.Number($1) }
  | SYMBOL { Type.Symbol($1) }
  | TRUE { Type.Bool true }
  | FALSE { Type.Bool false }
  | STRING { Type.String($1) }
  | LPAREN IF expr expr expr RPAREN { Type.If($3, $4, $5) }
  | LPAREN list RPAREN { $2 }

list:
    expr DOT expr { Type.Cons($1, $3) }
  | expr list { Type.Cons($1, $2) }
  | { Type.Nil }

