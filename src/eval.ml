open Type

exception Analyze_error of string
let error message = raise (Analyze_error message)

(* S-expression *)

let rec eval env expr =
  match expr with
  | Nil
  | Number _
  | Bool _
  | String _
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

(* scenario *)

type frame = {
  scripts: Obj.t array
}
[@@bs.deriving jsConverter]

type context = {
  frames: frame array;
  scripts: (Obj.t array) ref
}

type result = {
  frames: frame array
}

let eval_frame context env args =
  context.scripts := [||];
  let expr = eval env args in
  context.frames
  |> Js.Array.push ({ scripts = !(context.scripts) })
  |> ignore;
  expr

let eval_text context _ args =
  let text clear = function
    | Cons(String value, Nil) -> begin
      if clear then begin
        !(context.scripts)
        |> Js.Array.push (Obj.repr [%bs.obj {
          tag = "removeLayer";
          data = {
            name = "choice"
          }
        }])
        |> ignore
      end;
      !(context.scripts)
      |> Js.Array.push (Obj.repr [%bs.obj {
        tag = "text";
        data = {
          clear = Js.Boolean.to_js_boolean clear;
          values = [|value|]
        }
      }])
      |> ignore;
      Nil
    end
    | expr -> error ("eval_text: " ^ (to_str expr)) in
  match car args with
  | Cons(Symbol "clear", Cons(Bool v, Nil)) -> text v (cdr args)
  | _ -> text false args

let init_env context =
  let env = ref [] in
  let add_primitive name proc = Env.set env name (Primitive proc) in

  add_primitive "frame" (eval_frame context);
  add_primitive "text" (eval_text context);

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

