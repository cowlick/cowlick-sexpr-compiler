open Printf

exception Type_error of string

type expr =
  | Nil
  | Number of float
  | Symbol of string
  | Bool of bool
  | Cons of expr * expr
  | If of expr * expr * expr
  | Lambda of expr * expr * (symbol list) ref
  | Primitive of (expr -> (symbol list) ref -> expr)

and symbol_value = Value of expr

and symbol = {
  name: string;
  mutable value: expr
}

let rec print x =
  match x with
    Nil -> print_string "nil"
  | Number v -> printf "number %f" v
  | Symbol s -> printf "symbol %s" s
  | Bool v -> printf "bool %s" (if v then "#t" else "#f")
  | Cons(car, cdr) ->
    printf "(";
    print car;
    printf " . ";
    print cdr;
    printf ")"
  | If(cond, e1, e2) ->
    printf "if [";
    print cond;
    printf " ] -> ";
    print e1;
    printf " | ";
    print e2
  | Lambda(vars, body, env) -> printf "#proc"
  | Primitive _ -> printf "#primitive-proc"

