exception Translate_error of string

val translate: Type.t -> Obj.t
val assign: string -> string -> Type.t -> Obj.t
val return_body: Type.t -> Obj.t
