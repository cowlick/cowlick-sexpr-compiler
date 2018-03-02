open Printf

type t =
  | Nil
  | Number of float
  | Symbol of string * Location.t
  | Bool of bool
  | String of string
  | InterpolatedString of (t Ast.t) array
  | Embedded of Obj.t
  | Cons of t Ast.t * t Ast.t
  | Primitive of (((t Env.environment) list) ref -> Location.t -> t -> t)

let rec to_str x =
  match x with
    Nil -> "nil"
  | Number v -> sprintf "number %f" v
  | Symbol(s, _) -> sprintf "symbol %s" s
  | Bool v -> sprintf "bool %s" (if v then "#t" else "#f")
  | String v -> sprintf "string \"%s\"" v
  | InterpolatedString exprs ->
    exprs
    |> Array.fold_left (fun s t ->
      let str = t |> Ast.kind_of |> to_str in
      if s = "" then str
      else sprintf "%s %s" s str
    ) ""
  | Embedded _ -> "embedded variable"
  | Cons(car, cdr) ->
    sprintf "(%s . %s)" (car |> Ast.kind_of |> to_str) (cdr |> Ast.kind_of |> to_str)
  | Primitive _ -> "#primitive-proc"

let car = function
  Cons(e1, _) -> Ast.kind_of e1
| expr -> failwith ("constructor is not cons: " ^ (to_str expr))

let cdr = function
  Cons(_, e2) -> Ast.kind_of e2
| expr -> failwith ("constructor is not cons: " ^ (to_str expr))

let fold_left f seed expr =
  let rec inner acc = function
  | Cons(x, xs) -> inner (x |> Ast.kind_of |> f acc) (Ast.kind_of xs)
  | _ -> acc
  in inner seed expr

let pairwise expr =
  let rec inner acc v = function
  | Cons({ kind = x; loc = _ }, { kind = xs; loc = _ }) -> begin
    acc |> Js.Array.push (v, x) |> ignore;
    inner acc x xs
  end
  | _ -> acc in
  match expr with
  | Cons({ kind = x; loc = _ }, { kind = (Cons(_, _) as xs); loc = _ }) -> inner [||] x xs
  | _ -> [||]

