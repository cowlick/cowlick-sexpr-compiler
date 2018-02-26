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

let env: (((Type.t -> Obj.t) Env.environment) list) ref = ref []

let rec translate expr =
  let open Type in
  match expr with
  | Number v -> number_ast v
  | Bool  v -> bool_ast v
  | String v -> str_ast v
  | Cons({ kind = Symbol s; loc = _ }, { kind = args; loc = _ }) as exprs -> begin
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
  let open Type in
  let rec inner acc = function
  | Cons({ kind = x; loc = _ }, { kind = xs; loc = _ }) -> inner (translate x :: acc) xs
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
  |> Obj.repr

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
  |> Obj.repr

let arithmetic operator unary identity expr =
  let open Type in
  match expr with
  | Cons({ kind = e; loc = _ }, { kind = Nil; loc = _ }) -> unary operator (translate e)
  | Cons({ kind = left; loc = _ }, { kind = xs; loc = _ }) ->
    Type.fold_left (fun acc x -> binary operator acc (translate x)) (translate left) xs
  | Nil -> begin
    match identity with
    | Some e -> e
    | None -> error (sprintf "invalid arithmetic operator [%s]: %s" operator (to_str Nil))
  end
  | e -> error (sprintf "invalid arithmetic operator [%s]: %s" operator (to_str e));;

let arithmetic_unary operator identity expr =
  arithmetic operator (fun op expr -> unary op expr) identity expr

let arithmetic_binary operator identity expr =
  arithmetic operator (fun op expr -> binary op (number_ast 1.) expr) identity expr

let true_ast = bool_ast true

let reduce f xs =
  if Js.Array.length xs = 0 then raise (Invalid_argument "array is empty")
  else Js.Array.reduce f (xs.(0)) (Js.Array.sliceFrom 1 xs)

let comparison operator expr =
  let open Type in
  match expr with
  | Nil
  | Cons(_, { kind = Nil; loc = _ }) -> true_ast
  | Cons(_, _) as exprs ->
    exprs
    |> Type.pairwise
    |> Js.Array.map (fun (left, right) -> binary operator (translate left) (translate right))
    |> reduce (binary "&&")
  | e -> error (sprintf "invalid comparison operator [%s]: %s" operator (to_str e))

let and_or operator seed expr =
  let open Type in
  match expr with
  | Cons({ kind = expr; loc = _ }, { kind = Nil; loc = _ }) -> translate expr
  | Cons({ kind = left; loc = _ }, { kind = xs; loc = _ }) ->
    Type.fold_left (fun acc x -> binary operator acc (translate x)) (translate left) xs
  | Nil -> seed
  | e -> error (sprintf "invalid logical operator [%s]: %s" operator (to_str e))

let bang expr =
  let open Type in
  match expr with
  | Cons({ kind = e; loc = _ }, { kind = Nil; loc = _ }) -> unary "!" (translate e)
  | e -> error ("invalid logical operator [!]: " ^ (to_str e))

let slot_set expr =
  let open Type in
  match expr with
  | Cons({ kind = Symbol typ; loc = _ }, { kind = Cons({ kind = Symbol name; loc = _ }, { kind = Cons({ kind = e; loc = _ }, _); loc = _ }); loc = _ }) ->
    assign typ name e
    |> Obj.repr
  | e -> error (sprintf "invalid variable setting: %s" (to_str e))

let slot_ref expr =
  let open Type in
  match expr with
  | Cons({ kind = Symbol typ; loc = _ }, { kind = Cons({ kind = Symbol name; loc = _ }, _); loc = _ }) ->
    member (member (identifier "variables") (identifier typ)) (identifier name)
    |> Obj.repr
  | e -> error (sprintf "invalid variable reference: %s" (to_str e));;

Env.set env "+" (arithmetic_unary "+" (Some (number_ast 0.)));;
Env.set env "-" (arithmetic_unary "-" None);;
Env.set env "*" (arithmetic_binary "*" (Some (number_ast 1.)));;
Env.set env "/" (arithmetic_binary "/" None);;
Env.set env "and" (and_or "&&" true_ast);;
Env.set env "or" (and_or "||" (bool_ast false));;
Env.set env "not" bang;;
[|
  ("=", "===");
  ("<", "<");
  (">", ">");
  ("<=", "<=");
  (">=", ">=");
  ("string=?", "===")
|]
|> Array.iter (fun (x, y) -> Env.set env x (comparison y));;
Env.set env "slot-set!" slot_set;;
Env.set env "set!" slot_set;;
Env.set env "slot-ref" slot_ref;;
Env.set env "ref" slot_ref;;

