exception Eval_error of string

type frame = <
  label: string Js.Nullable.t;
  scripts: Script.t array
> Js.t

type eval_result = {
  frames: frame array;
  dependencies: string array
}

val eval_scene: base:string -> relative:string -> Type.t Ast.t -> eval_result

