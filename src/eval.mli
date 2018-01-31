exception Analyze_error of string

type frame = {
  scripts: Obj.t array
}
[@@bs.deriving jsConverter]

type result = {
  frames: frame array
}

val eval_scene: Type.expr -> result

