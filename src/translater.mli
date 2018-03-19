exception Translate_error of string

val translate: Type.t -> Estree.t
val assign: string -> string -> Type.t -> Estree.t
val return_body: Type.t -> Estree.t
