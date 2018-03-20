exception Eval_error of string

type eval_result = {
  frames: Core.frame array;
  dependencies: string array
}

val eval_scene: base:string -> relative:string -> Type.t Ast.t -> eval_result

