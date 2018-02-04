open Type
open Printf

exception Analyze_error of string
let error message = raise (Analyze_error message)

let rec eval env expr =
  match expr with
  | Nil
  | Number _
  | Bool _
  | String _
  | InterpolatedString _
  | Embedded _
  | Primitive _ -> expr
  | Symbol s -> Env.lookup !env s
  | Cons(e1, e2) -> begin
    match eval env e1 with
    | Primitive proc -> proc env e2
    | Nil -> Nil
    | other -> error ("uneval app: " ^ (to_str other))
  end
  | If(cond, expr1, expr2) -> eval_if cond expr1 expr2 env

and eval_if cond expr1 expr2 env =
  match eval env cond with
  | Bool true -> eval env expr1
  | _ -> eval env expr2

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
  frames: frame array
}

let eval_frame context env args =
  context.scripts := [||];
  let expr = eval env args in
  context.frames
  |> Js.Array.push ([%bs.obj { scripts = !(context.scripts) } ])
  |> ignore;
  expr

let push_tag scripts name (data: Obj.t) =
  scripts
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
      |> push_tag !(context.scripts) "removeLayer"
      |> ignore
  in let text clear = function
    | Cons(String value, Nil) -> begin
      add_clear clear;
      [%bs.obj {
        clear = Js.Boolean.to_js_boolean clear;
        values = [|value|]
      }]
      |> Obj.repr
      |> push_tag !(context.scripts) "text"
    end
    | Cons(InterpolatedString exprs, Nil) -> begin
      add_clear clear;
      [%bs.obj {
        clear = Js.Boolean.to_js_boolean clear;
        values =
          exprs
          |> Array.map (fun expr ->
            match eval env expr with
            | String s -> Obj.repr s
            | Embedded v -> Obj.repr v
            | e -> error ("invalid interpolated string: " ^ (to_str e))
          )
      }]
      |> Obj.repr
      |> push_tag !(context.scripts) "text"
    end
    | expr -> error ("eval_text: " ^ (to_str expr)) in
  match car args with
  | Cons(Symbol "clear", Cons(Bool v, Nil)) -> text v (cdr args)
  | _ -> text false args

let eval_variable typ _ args =
  match car args with
  | Symbol name ->
    Embedded (Obj.repr [%bs.obj {
      _type = typ;
      name = name
    }])
  | e -> error (sprintf "invalid %s variable name: %s" typ (to_str e))

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
    |> push_tag !(context.scripts) "image"
  | (e, _) -> error (sprintf "image tag requires asset id: %s" (to_str e))

let init_env context =
  let env = ref [] in
  let add_primitive name proc = Env.set env name (Primitive proc) in

  add_primitive "frame" (eval_frame context);
  add_primitive "text" (eval_text context);
  add_primitive "system" (eval_variable "system");
  add_primitive "current" (eval_variable "current");
  add_primitive "image" (eval_image context);
  add_primitive "layer" eval_layer;

  env

let eval_scene expr =
  let context = {
    frames = [||];
    scripts = ref [||]
  } in
  let _ = eval (init_env context) expr in
  {
    frames = context.frames
  }

