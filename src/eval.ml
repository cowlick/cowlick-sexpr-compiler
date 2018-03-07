open Type
open Printf

exception Eval_error of string
let error (loc: Location.t) message expr =
  let value = sprintf "[Line: %d, Column: %d]%s:\n%s" loc.begin_line loc.begin_column message (to_str expr) in
  raise (Eval_error value)

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
  | Symbol(s, l1) -> begin
    match Env.lookup !env s with
    | Some p -> p
    | None -> Primitive(fun e l2 args ->
      match Env.lookup !e user_defined_env_name with
      | Some (Primitive proc) ->
        proc e l1 (Cons({ kind = expr; loc = l1 }, { kind = args; loc = l2 }))
      | Some other -> error l1 "uneval app" other
      | None -> error l1 "not found in env" expr
    )
  end
  | Cons({ kind = e1; loc = l1 }, { kind = e2; loc = l2 }) -> begin
    match eval env e1 with
    | Primitive proc -> proc env l2 e2
    | Nil -> eval env e2
    | other -> error l1 "uneval app" other
  end

type script = <
  tag: string;
  data: Obj.t
> Js.t

type frame = <
  label: string Js.Nullable.t;
  scripts: script array
> Js.t

type context = {
  frames: frame array;
  scripts: (script array) ref;
  dependencies: string array
}

type eval_result = {
  frames: frame array;
  dependencies: string array
}

let eval_frame context env loc args =
  let rec inner = function
  | Cons({ kind = x; loc = _ }, { kind = xs; loc = _ }) ->
    eval env x |> ignore;
    inner xs |> ignore;
  | Nil -> ()
  | e -> error loc "invalid frame arguments" e in
  let label, xs =
    match args with
    | Cons({ kind = x; loc = _ }, { kind = xs; loc = _ }) -> begin
      match x with
      | Cons({ kind = Symbol("label", _); loc = _ }, { kind = Cons({ kind = (Symbol(v, _) | String v); loc = _}, { kind = Nil; loc = _ }); loc = _ }) ->
        (Js.Nullable.return v, xs)
      | _ -> (Js.Nullable.undefined, args)
    end
    | _ -> (Js.Nullable.undefined, args) in
  context.scripts := [||];
  inner xs;
  context.frames
  |> Js.Array.push ([%bs.obj {
    label;
    scripts = !(context.scripts)
  }])
  |> ignore;
  Nil

let push_tag context name (data: 'a) =
  !(context.scripts)
  |> Js.Array.push ([%bs.obj {
    tag = name;
    data = Obj.repr data
  }])
  |> ignore;
  Nil

let eval_text context env loc args =
  let add_clear clear =
    if clear then
      [%bs.obj {
        name = "choice"
      }]
      |> push_tag context "removeLayer"
      |> ignore
  in let text clear = function
    | Cons({ kind = String value; loc = _ }, { kind = Nil; loc = _ }) -> begin
      add_clear clear;
      [%bs.obj {
        clear = Js.Boolean.to_js_boolean clear;
        values = [|value|]
      }]
      |> push_tag context "text"
    end
    | Cons({ kind = InterpolatedString exprs; loc = _ }, {kind = Nil; loc = _ }) -> begin
      add_clear clear;
      [%bs.obj {
        clear = Js.Boolean.to_js_boolean clear;
        values =
          Array.fold_right (fun ast acc ->
            match ast |> Ast.kind_of |> eval env with
            | String s -> if s = "" then acc else Obj.repr s :: acc
            | Embedded v -> v :: acc
            | e -> error (Ast.loc_of ast) "invalid interpolated string" e
          ) exprs []
          |> Array.of_list
      }]
      |> push_tag context "text"
    end
    | expr -> error loc "invalid text" expr in
  match car args with
  | Cons({ kind = Symbol("clear", _); loc = _ }, { kind = Cons({ kind = Bool v; loc = _ }, { kind = Nil; loc = _ }); loc = _ }) -> text v (cdr args)
  | _ -> text false args

let eval_slot_set context _ loc args =
  match args with
  | Cons({ kind = Symbol(typ, _); loc = _ }, { kind = Cons({ kind = Symbol(name, _); loc = _ }, { kind = Cons({ kind = expr; loc = _ }, _); loc = _ }); loc = _ }) ->
    Js_ast.assign typ name expr
    |> push_tag context "eval"
  | e -> error loc "invalid variable setting" e

let eval_slot_ref _ loc args =
  match args with
  | Cons({ kind = Symbol(typ, _); loc = _ }, { kind = Cons({ kind = Symbol(name, _); loc = _ }, _); loc = _ }) ->
    Embedded (Obj.repr [%bs.obj {
      _type = typ;
      name = name
    }])
  | e -> error loc "invalid variable reference" e

let eval_layer _ loc args =
  let layer = Js.Dict.empty () in
  let rec inner = function
  | Cons({ kind = Symbol(key, _); loc = _ }, { kind = xs; loc = l }) ->
    let value =
      match key, car xs with
      | ("name", String v) -> Obj.repr v
      | (("x" | "y"), Number v) -> Obj.repr v
      | (_, e) -> error l ("type error [" ^ key ^ "]") e
    in
      Js.Dict.set layer key value |> ignore;
      inner (cdr xs)
  | Cons({ kind = x; loc = _ }, { kind = xs; loc = _ }) ->
    inner x;
    inner xs
  | Nil -> ()
  | e -> error loc "invalid layer option" e
  in
    inner args;
    Embedded(Obj.repr layer)

let eval_image context env loc args =
  match car args, eval env (car (cdr args)) with
  | (String assetId, Embedded layer) ->
    [%bs.obj {
      assetId = assetId;
      layer = layer
    }]
    |> push_tag context "image"
  | (e, _) -> error loc "image tag requires asset id" e

let eval_if context env loc args =
  match args with
  | Cons({ kind = cond; loc = _ }, { kind = Cons({ kind = expr1; loc = _ }, { kind = Cons(expr2, _); loc = _ }); loc = _ }) -> begin
    let current = !(context.scripts) in
    context.scripts := [||];
    eval env expr1 |> ignore;
    let e1 = !(context.scripts) in
    context.scripts := [||];
    eval env (Ast.kind_of expr2) |> ignore;
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
    |> push_tag context "ifElse"
  end
  | e -> error loc "syntax error [if cond if-body else-body]" e

let eval_cond context env loc args =
  let conditions = [||] in
  let rec inner = function
  | Cons({ kind = Cons({ kind = Symbol("else", _); loc = _ }, { kind = Cons({ kind = expr; loc = _ }, { kind = Nil; loc = _ }); loc = _ }); loc = _ }, { kind = Nil; loc = _ }) -> begin
    context.scripts := [||];
    eval env expr |> ignore;
    !(context.scripts)
  end
  | Cons({ kind = Cons({ kind = pred; loc = _}, { kind = Cons({ kind = expr; loc = _ }, { kind = Nil; loc = _}); loc = _ }); loc = _ }, { kind = xs; loc = _ }) -> begin
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
  | e -> error loc "syntax error [cond (cond1 expr-1) ... (else expr-n)]" e
  in
    let current = !(context.scripts) in
    let e = inner args in
    context.scripts := current;
    [%bs.obj {
      conditions = conditions;
      elseBody = e
    }]
    |> push_tag context "ifElse"

let eval_ruby _ loc args =
  let options = Js.Dict.empty () in
  let rec inner = function
    | Cons({ kind = Symbol(key, _); loc = _ }, { kind = xs; loc = _ }) ->
    let value =
      match key, car xs with
      | (("rb" | "rt"), String v) -> v
      | (_, e) -> error loc ("type error [" ^ key ^ "]") e
    in
      Js.Dict.set options key value |> ignore;
      inner (cdr xs)
  | Cons(x, xs) ->
    x |> Ast.kind_of |> inner;
    xs |> Ast.kind_of |> inner
  | Nil -> ()
  | e -> error loc "invalid ruby option" e
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

let eval_jump (context: context) _ loc args =
  let target = Js.Dict.empty () in
  let rec inner loc = function
  | Cons({ kind = Symbol(key, _); loc = _ }, { kind = xs; loc = l }) ->
    let k, v =
      match key, car xs with
      | ("scene", (String v | Symbol(v, _))) -> begin
        Js.Array.push v context.dependencies |> ignore;
        (key, Obj.repr v)
      end
      | ("label", (String v | Symbol(v, _))) -> ("frame", Obj.repr v)
      | (_, e) -> error l ("unknwon argument [" ^ key ^ "]") e
    in
      Js.Dict.set target k v |> ignore;
      inner loc (cdr xs)
  | Cons({ kind = x; loc = l1 }, { kind = xs; loc = l2 }) ->
    inner l1 x;
    inner l2 xs
  | Nil -> ()
  | e -> error loc ("invalid jump option") e
  in
    inner loc args;
    push_tag context "jump" target

let eval_user_defined context _ loc args =
  let layer = Js.Dict.empty () in
  let rec inner name loc = function
  | Cons({ kind = Symbol(key, _); loc = _ }, { kind = xs; loc = l }) ->
    let value =
      match car xs with
      | Bool v -> Obj.repr v
      | String v -> Obj.repr v
      | Number v -> Obj.repr v
      | e -> error l ("type error [" ^ key ^ "]") e
    in
      Js.Dict.set layer key value |> ignore;
      inner name loc (cdr xs)
  | Cons({ kind = x; loc = l1 }, { kind = xs; loc = l2 }) ->
    inner name l1 x;
    inner name l2 xs
  | Nil -> ()
  | e -> error loc ("invalid option in [" ^ name ^ "]") e
  in
    match args with
    | Cons({ kind = Symbol(name, _); loc = _ }, { kind = xs; loc = l }) ->
      inner name l xs;
      push_tag context name layer
    | e -> error loc "invalid operation" e

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
  add_primitive "jump" (eval_jump context);
  add_primitive user_defined_env_name (eval_user_defined context);

  env

let eval_scene ast =
  let context = {
    frames = [||];
    scripts = ref [||];
    dependencies = [||]
  } in
  let _ = eval (init_env context) (Ast.kind_of ast) in
  {
    frames = context.frames;
    dependencies = context.dependencies
  }

