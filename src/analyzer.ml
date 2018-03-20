class type _inline_script = object
  method generate: unit -> string
  method write: string -> unit Js.Promise.t
end [@bs]

type inline_script = _inline_script Js.t

type generated_scene = <
  label: string;
  source: Estree.t
> Js.t

type analyze_result = <
  scenario: generated_scene array;
  scripts: inline_script array
> Js.t

external filename: string -> string = "" [@@bs.val] [@@bs.module "cowlick-analyzer"]
external analyze: Core.scenario -> analyze_result = "" [@@bs.val] [@@bs.module "cowlick-analyzer"]
external generate: string -> generated_scene array -> unit Js.Promise.t = "" [@@bs.val] [@@bs.module "cowlick-analyzer"]

