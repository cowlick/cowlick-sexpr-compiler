open Printf

type expr =
  | Nil
  | Number of float
  | Symbol of string
  | Bool of bool
  | String of string
  | InterpolatedString of expr array
  | Embedded of Obj.t
  | Cons of expr * expr
  | Primitive of (((expr Env.environment) list) ref -> expr -> expr)

let rec to_str x =
  match x with
    Nil -> "nil"
  | Number v -> sprintf "number %f" v
  | Symbol s -> sprintf "symbol %s" s
  | Bool v -> sprintf "bool %s" (if v then "#t" else "#f")
  | String v -> sprintf "string \"%s\"" v
  | InterpolatedString exprs ->
    exprs
    |> Array.fold_left (fun s e ->
      if s = "" then to_str e
      else sprintf "%s %s" s (to_str e)
    ) ""
  | Embedded _ -> "embedded variable"
  | Cons(car, cdr) ->
    sprintf "(%s . %s)" (to_str car) (to_str cdr)
  | Primitive _ -> "#primitive-proc"

let car = function
    Cons(e1, _) -> e1
  | _ -> Nil

let cdr = function
    Cons(_, e2) -> e2
  | _ -> Nil

let fold_left f seed expr =
  let rec inner acc = function
  | Cons(x, xs) -> inner (f acc x) xs
  | _ -> acc
  in inner seed expr

let pairwise expr =
  let rec inner acc v = function
  | Cons(x, xs) -> begin
    acc |> Js.Array.push (v, x) |> ignore;
    inner acc x xs
  end
  | _ -> acc in
  match expr with
  | Cons(x, (Cons(_, _) as xs)) -> inner [||] x xs
  | _ -> [||]

