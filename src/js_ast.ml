open Type
open Printf

exception Translate_error of string
let error message = raise (Translate_error message)

let literal value raw =
  [%bs.obj {
    _type = "Literal";
    value = value;
    raw = raw
  }]
  |> Obj.repr

let number_ast value =
  literal (Obj.repr value) (sprintf "%f" value)

let bool_ast value =
  literal (Obj.repr (Js.Boolean.to_js_boolean value)) (sprintf "%b" value)

let str_ast str =
  literal (Obj.repr str) (sprintf "\"%s\"" str)

let identifier name =
  [%bs.obj {
    _type = "Identifier";
    name = name
  }]
  |> Obj.repr

let env: (((expr -> Obj.t) Env.environment) list) ref = ref []

let rec translate = function
| Number v -> number_ast v
| Bool  v -> bool_ast v
| String v -> str_ast v
| Cons(Symbol s, args) as exprs -> begin
  match Env.lookup !env s with
  | Some f -> f args
  | None -> begin
    match args with
    | Cons(_, _) -> call s args
    | Nil -> identifier s
    | _ -> error ("illegal call: " ^ (to_str exprs))
  end
end
| expr -> error ("not implemented: %s" ^ (to_str expr))

and call name exprs =
  let rec inner acc = function
  | Cons(x, xs) -> inner (translate x :: acc) xs
  | Nil -> List.rev acc
  | expr -> translate expr :: acc |> List.rev
  in [%bs.obj {
    _type = "CallExpression";
    callee = identifier name;
    arguments = inner [] exprs |> Array.of_list
  }]
  |> Obj.repr

let unary operator arg =
  [%bs.obj {
    _type = "UnaryExpression";
    operator = operator;
    argument = arg
  }]
  |> Obj.repr

let binary operator left right =
  [%bs.obj {
    _type = "BinaryExpression";
    operator = operator;
    left = left;
    right = right
  }]
  |> Obj.repr

let member receiver property =
  [%bs.obj {
    _type = "MemberExpression";
    _object = receiver;
    property = property;
    computed = Js.false_
  }]

let assign typ name expr =
  [%bs.obj {
    _type = "Program";
    body = [|
      {
        _type = "ExpressionStatement";
        expression = {
          _type = "AssignmentExpression";
          operator = "=";
          left = member (member (identifier "variables") (identifier typ)) (identifier name);
          right = translate expr
        }
      }
    |];
    sourceType = "script"
  }]

let return_body expr =
  [%bs.obj {
    _type = "Program";
    body = [|
      {
        _type = "ExpressionStatement";
        expression = translate expr
      }
    |];
    sourceType = "script"
  }]

let arithmetic operator unary identity = function
| Cons(left, Cons(right, Nil)) -> binary operator (translate left) (translate right)
| Cons(expr, Nil) -> unary operator (translate expr)
| Nil -> begin
  match identity with
  | Some expr -> expr
  | None -> error (sprintf "invalid arithmetic operator [%s]: %s" operator (to_str Nil))
end
| e -> error (sprintf "invalid arithmetic operator [%s]: %s" operator (to_str e));;

Env.set env "+" (arithmetic "+" (fun op expr -> unary op expr) (Some (number_ast 0.)));;
Env.set env "-" (arithmetic "-" (fun op expr -> unary op expr) None);;
Env.set env "*" (arithmetic "*" (fun op expr -> binary op (number_ast 1.) expr) (Some (number_ast 1.)));;
Env.set env "/" (arithmetic "/" (fun op expr -> binary op (number_ast 1.) expr) None);;

