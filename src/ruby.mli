type t = <
  rb: string;
  rt: string
> Js.t

val to_json: t -> string
val generate: string -> string -> t array
