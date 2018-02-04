exception Analyze_error of string

type script = <
  tag: string;
  data: Obj.t
> Js.t

type frame = <
  scripts: script array
> Js.t

type eval_result = {
  frames: frame array
}

val eval_scene: Type.expr -> eval_result

