type ruby = <
  rb: string;
  rt: string
> Js.t

val to_json: ruby -> string
val generate: string -> string -> ruby array
