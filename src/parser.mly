%{
%}

%token <float> NUMBER
%token <string> SYMBOL
%token <string> STRING
%token <string> INTERPOLATED_STRING
%token <string> INTERPOLATED_STRING_END
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
  | interpolated_string { $1 }
  | STRING { Type.String($1) }
  | LPAREN IF expr expr expr RPAREN { Type.If($3, $4, $5) }
  | LPAREN list RPAREN { $2 }

list:
    expr DOT expr { Type.Cons($1, $3) }
  | expr list { Type.Cons($1, $2) }
  | { Type.Nil }

interpolated_string:
  | INTERPOLATED_STRING interpolations INTERPOLATED_STRING_END {
    let xs = [| Type.String($1) |] in
    $2 |> Js.Array.forEach(fun x -> Js.Array.push x xs |> ignore);
    xs |> Js.Array.push (Type.String($3)) |> ignore;
    Type.InterpolatedString(xs)
  }
  | INTERPOLATED_STRING_END {
    Type.InterpolatedString([| Type.String($1) |])
  }

interpolations:
    interpolation { [| $1 |] }
  | interpolations INTERPOLATED_STRING interpolation {
    let xs = $1 in
    Js.Array.push (Type.String($2)) xs |> ignore;
    Js.Array.push $3 xs |> ignore;
    xs
  }

interpolation: expr { $1 }

