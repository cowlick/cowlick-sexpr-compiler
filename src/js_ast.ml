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
| expr -> error ("not implemented: " ^ (to_str expr))

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
| Cons(expr, Nil) -> unary operator (translate expr)
| Cons(left, xs) ->
  Type.fold_left (fun acc x -> binary operator acc (translate x)) (translate left) xs
| Nil -> begin
  match identity with
  | Some expr -> expr
  | None -> error (sprintf "invalid arithmetic operator [%s]: %s" operator (to_str Nil))
end
| e -> error (sprintf "invalid arithmetic operator [%s]: %s" operator (to_str e));;

let arithmetic_unary operator identity expr =
  arithmetic operator (fun op expr -> unary op expr) identity expr

let arithmetic_binary operator identity expr =
  arithmetic operator (fun op expr -> binary op (number_ast 1.) expr) identity expr

let and_or operator seed = function
| Cons(expr, Nil) -> translate expr
| Cons(left, xs) ->
  Type.fold_left (fun acc x -> binary operator acc (translate x)) (translate left) xs
| Nil -> seed
| e -> error (sprintf "invalid comparison operator [%s]: %s" operator (to_str e))

let bang = function
| Cons(expr, Nil) -> unary "!" (translate expr)
| e -> error ("invalid logical not: " ^ (to_str e));;

Env.set env "+" (arithmetic_unary "+" (Some (number_ast 0.)));;
Env.set env "-" (arithmetic_unary "-" None);;
Env.set env "*" (arithmetic_binary "*" (Some (number_ast 1.)));;
Env.set env "/" (arithmetic_binary "/" None);;
Env.set env "and" (and_or "&&" (bool_ast true));;
Env.set env "or" (and_or "||" (bool_ast false));;
Env.set env "not" bang;;

