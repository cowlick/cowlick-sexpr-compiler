class type _inline_script = object
  method generate: unit -> string
  method write: string -> unit Js.Promise.t
end [@bs]

type inline_script = _inline_script Js.t

type analyze_result = <
  scenario: Obj.t;
  scripts: inline_script array
> Js.t

external filename: string -> string = "" [@@bs.val] [@@bs.module "cowlick-analyzer"]
external analyze: Obj.t -> analyze_result = "" [@@bs.val] [@@bs.module "cowlick-analyzer"]
external generate: string -> Obj.t -> unit Js.Promise.t = "" [@@bs.val] [@@bs.module "cowlick-analyzer"]

