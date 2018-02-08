exception Translate_error of string

val translate: Type.expr -> Obj.t
val assign: string -> string -> Type.expr -> Obj.t
val return_body: Type.expr -> Obj.t
