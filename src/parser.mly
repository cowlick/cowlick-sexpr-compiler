%{

let to_loc s_pos e_pos =
  let open Location in
  let open Lexing in
  {
    begin_column = s_pos.pos_cnum;
    begin_line = s_pos.pos_lnum;
    begin_bol = s_pos.pos_cnum - s_pos.pos_bol;
    end_column = e_pos.pos_cnum;
    end_line = e_pos.pos_lnum;
    end_bol = e_pos.pos_cnum - e_pos.pos_bol;
  }

let make kind s e =
  let loc = to_loc s e in
  Ast.{kind; loc}

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

%start <Type.t Ast.t> entry

%%
entry:
    expr EOF { $1 }

expr:
    primary_value { make $1 $startpos $endpos }
  | interpolated_string { $1 }
  | LPAREN list RPAREN { $2 }

primary_value:
    NUMBER { Type.Number $1 }
  | SYMBOL { Type.Symbol $1 }
  | TRUE { Type.Bool true }
  | FALSE { Type.Bool false }
  | STRING { Type.String $1 }

list:
    e1 = expr
    DOT
    e2 = expr
    {
      make (Type.Cons(e1, e2)) $startpos(e1) $endpos(e2)
    }
  |
    e = expr
    l = list
    {
      make (Type.Cons(e, l)) $startpos(e) $endpos(l)
    }
  | { make Type.Nil $startpos $endpos }

interpolated_string:
    INTERPOLATED_STRING
    is = interpolations
    INTERPOLATED_STRING_END
    {
      let s_start_pos = $startpos($1) in
      let s_end_pos = $endpos($1) in
      let e_start_pos = $startpos($3) in
      let e_end_pos = $endpos($3) in
      let xs = [| make (Type.String $1) s_start_pos s_end_pos |] in
      is |> Js.Array.forEach(fun x -> Js.Array.push x xs |> ignore);
      xs |> Js.Array.push (make (Type.String $3) e_start_pos e_end_pos) |> ignore;
      make (Type.InterpolatedString(xs)) s_start_pos e_end_pos
    }
  | INTERPOLATED_STRING_END {
    make (Type.InterpolatedString([| make (Type.String $1) $startpos $endpos |])) $startpos $endpos
  }

interpolations:
    interpolation { [| $1 |] }
  |
    is = interpolations
    s = INTERPOLATED_STRING
    i = interpolation
    {
      Js.Array.push (make (Type.String s) $startpos(s) $endpos(s)) is |> ignore;
      Js.Array.push i is |> ignore;
      is
    }

interpolation: expr { $1 }

