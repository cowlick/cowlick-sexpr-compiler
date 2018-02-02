open Analyzer
open Eval

type scene = <
  label: string;
  frames: frame array
> Js.t

external mkdirSync : string -> unit = "" [@@bs.val] [@@bs.module "fs"]

let parse_scene target =
  let result =
    target
    |> Node.Fs.readFileAsUtf8Sync
    |> Lexing.from_string
    |> Parser.expr Lexer.token
    |> eval_scene
  in [%bs.obj {
    label = filename target;
    frames = result.frames
  }]

let parse baseDir =
  let scenario = [||] in
  let scene =
    "first.scm"
    |> Filename.concat baseDir
    |> parse_scene in
  Js.Array.push scene scenario |> ignore;
  scenario

let compile outDir inputDir =
  let ast = parse inputDir in
  let result = analyze (Obj.repr ast) in
  let () =
    try mkdirSync outDir
    with _ -> Js.log("output directory already exists: " ^ outDir)
  in
  let out = Node.Path.join [| outDir; "scenario.js" |] in
    Array.map (fun s -> s#write outDir) result##scripts
    |> Array.append [| generate out result##scenario |]
    |> Js.Promise.all

