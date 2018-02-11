open Type
open Printf

exception Eval_error of string
let error message = raise (Eval_error message)

let user_defined_env_name = "#"

let rec eval env expr =
  match expr with
  | Nil
  | Number _
  | Bool _
  | String _
  | InterpolatedString _
  | Embedded _
  | Primitive _ -> expr
  | Symbol s -> begin
    match Env.lookup !env s with
    | Some p -> p
    | None -> Primitive(fun e args ->
      match Env.lookup !e user_defined_env_name with
      | Some (Primitive proc) -> proc e (Cons(Symbol s, args))
      | Some other -> error ("uneval app: " ^ (to_str other))
      | None -> error (sprintf "not found in env: %s" s)
    )
  end
  | Cons(e1, e2) -> begin
    match eval env e1 with
    | Primitive proc -> proc env e2
    | Nil -> Nil
    | other -> error ("uneval app: " ^ (to_str other))
  end

type script = <
  tag: string;
  data: Obj.t
> Js.t

type frame = <
  scripts: script array
> Js.t

type context = {
  frames: frame array;
  scripts: (script array) ref
}

type eval_result = {
  frames: frame array;
  dependencies: string array
}

let eval_frame context env args =
  let rec inner = function
  | Cons(x, xs) ->
    eval env x |> ignore;
    inner xs |> ignore;
  | Nil -> ()
  | e -> error (sprintf "invalid frame arguments: %s" (to_str e))
  in
    context.scripts := [||];
    inner args;
    context.frames
    |> Js.Array.push ([%bs.obj { scripts = !(context.scripts) } ])
    |> ignore;
    Nil

let push_tag context name (data: Obj.t) =
  !(context.scripts)
  |> Js.Array.push ([%bs.obj {
    tag = name;
    data = data
  }])
  |> ignore;
  Nil

let eval_text context env args =
  let add_clear clear =
    if clear then
      [%bs.obj {
        name = "choice"
      }]
      |> Obj.repr
      |> push_tag context "removeLayer"
      |> ignore
  in let text clear = function
    | Cons(String value, Nil) -> begin
      add_clear clear;
      [%bs.obj {
        clear = Js.Boolean.to_js_boolean clear;
        values = [|value|]
      }]
      |> Obj.repr
      |> push_tag context "text"
    end
    | Cons(InterpolatedString exprs, Nil) -> begin
      add_clear clear;
      [%bs.obj {
        clear = Js.Boolean.to_js_boolean clear;
        values =
          Array.fold_right (fun expr acc ->
            match eval env expr with
            | String s -> if s = "" then acc else Obj.repr s :: acc
            | Embedded v -> v :: acc
            | e -> error ("invalid interpolated string: " ^ (to_str e))
          ) exprs []
          |> Array.of_list
      }]
      |> Obj.repr
      |> push_tag context "text"
    end
    | expr -> error ("eval_text: " ^ (to_str expr)) in
  match car args with
  | Cons(Symbol "clear", Cons(Bool v, Nil)) -> text v (cdr args)
  | _ -> text false args

let eval_slot_set context _ args =
  match args with
  | Cons(Symbol typ, Cons(Symbol name, Cons(expr, _))) ->
    Js_ast.assign typ name expr
    |> Obj.repr
    |> push_tag context "eval"
  | e -> error (sprintf "invalid variable setting: %s" (to_str e))

let eval_slot_ref _ args =
  match args with
  | Cons(Symbol typ, Cons(Symbol name, _)) ->
    Embedded (Obj.repr [%bs.obj {
      _type = typ;
      name = name
    }])
  | e -> error (sprintf "invalid variable reference: %s" (to_str e))

let eval_layer _ args =
  let layer = Js.Dict.empty () in
  let rec inner = function
  | Cons(Symbol key, xs) ->
    let value =
      match key, car xs with
      | ("name", String v) -> Obj.repr v
      | ("x", Number v)
      | ("y", Number v) -> Obj.repr v
      | (_, e) -> error (sprintf "type error [%s]: %s" key (to_str e))
    in
      Js.Dict.set layer key value |> ignore;
      inner (cdr xs)
  | Cons(x, xs) ->
    inner x;
    inner xs
  | Nil -> ()
  | e -> error (sprintf "invalid layer option: %s" (to_str e))
  in
    inner args;
    Embedded(Obj.repr layer)

let eval_image context env args =
  match car args, eval env (car (cdr args)) with
  | (String assetId, Embedded layer) ->
    [%bs.obj {
      assetId = assetId;
      layer = layer
    }]
    |> Obj.repr
    |> push_tag context "image"
  | (e, _) -> error (sprintf "image tag requires asset id: %s" (to_str e))

and eval_if context env args =
  match args with
  | Cons(cond, Cons(expr1, Cons(expr2, _))) -> begin
    let current = !(context.scripts) in
    context.scripts := [||];
    eval env expr1 |> ignore;
    let e1 = !(context.scripts) in
    context.scripts := [||];
    eval env expr2 |> ignore;
    let e2 = !(context.scripts) in
    context.scripts := current;
    [%bs.obj {
      conditions = [|
        {
          expression = Js_ast.return_body cond;
          scripts = e1
        }
      |];
      elseBody = e2
    }]
    |> Obj.repr
    |> push_tag context "ifElse"
  end
  | e -> error (sprintf "syntax error [if cond if-body else-body]: %s" (to_str e))

let eval_cond context env args =
  let conditions = [||] in
  let rec inner = function
  | Cons(Cons(Symbol "else", Cons(expr, Nil)), Nil) -> begin
    context.scripts := [||];
    eval env expr |> ignore;
    !(context.scripts)
  end
  | Cons(Cons(pred, Cons(expr, Nil)), xs) -> begin
    context.scripts := [||];
    eval env expr |> ignore;
    conditions
    |> Js.Array.push [%bs.obj {
      expression = Js_ast.return_body pred;
      scripts = !(context.scripts)
    }]
    |> ignore;
    inner xs
  end
  | e -> error (sprintf "syntax error [cond (cond1 expr-1) ... (else expr-n)]:\n %s" (to_str e))
  in
    let current = !(context.scripts) in
    let e = inner args in
    context.scripts := current;
    [%bs.obj {
      conditions = conditions;
      elseBody = e
    }]
    |> Obj.repr
    |> push_tag context "ifElse"

let eval_ruby _ args =
  let options = Js.Dict.empty () in
  let rec inner = function
  | Cons(Symbol key, xs) ->
    let value =
      match key, car xs with
      | ("rb", String v) -> v
      | ("rt", String v) -> v
      | (_, e) -> error (sprintf "type error [%s]: %s" key (to_str e))
    in
      Js.Dict.set options key value |> ignore;
      inner (cdr xs)
  | Cons(x, xs) ->
    inner x;
    inner xs
  | Nil -> ()
  | e -> error ("invalid ruby option: " ^ (to_str e))
  in
    inner args;
    let rb = Js.Dict.unsafeGet options "rb" in
    let rt = Js.Dict.unsafeGet options "rt" in
    let result =
      Ruby.generate rb rt
      |> Array.map (fun v ->
        [%bs.obj {
          value = Ruby.to_json v
        }]
      )
    in Embedded (Obj.repr result)

let eval_user_defined context _ args =
  let layer = Js.Dict.empty () in
  let rec inner name = function
  | Cons(Symbol key, xs) ->
    let value =
      match car xs with
      | Bool v -> Obj.repr v
      | String v -> Obj.repr v
      | Number v -> Obj.repr v
      | e -> error (sprintf "type error [%s]: %s" key (to_str e))
    in
      Js.Dict.set layer key value |> ignore;
      inner name (cdr xs)
  | Cons(x, xs) ->
    inner name x;
    inner name xs
  | Nil -> ()
  | e -> error (sprintf "invalid option in [%s]: %s" name (to_str e))
  in
    match args with
    | Cons(Symbol name, xs) ->
      inner name xs;
      Obj.repr layer
      |> push_tag context name
    | e -> error (sprintf "invalid operation: %s" (to_str e))

let init_env context =
  let env = ref [] in
  let add_primitive name proc = Env.set env name (Primitive proc) in

  add_primitive "frame" (eval_frame context);
  add_primitive "text" (eval_text context);
  add_primitive "slot-set!" (eval_slot_set context);
  add_primitive "set!" (eval_slot_set context);
  add_primitive "slot-ref" eval_slot_ref;
  add_primitive "ref" eval_slot_ref;
  add_primitive "image" (eval_image context);
  add_primitive "layer" eval_layer;
  add_primitive "if" (eval_if context);
  add_primitive "cond" (eval_cond context);
  add_primitive "ruby" eval_ruby;
  add_primitive user_defined_env_name (eval_user_defined context);

  env

let eval_scene expr =
  let context = {
    frames = [||];
    scripts = ref [||]
  } in
  let _ = eval (init_env context) expr in
  {
    frames = context.frames;
    dependencies = [||]
  }

