exception Analyze_error of string

type frame = <
  scripts: Obj.t array
> Js.t

type eval_result = {
  frames: frame array
}

val eval_scene: Type.expr -> eval_result

