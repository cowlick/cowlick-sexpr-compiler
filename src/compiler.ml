open Analyzer
open Eval

type scene = <
  label: string;
  frames: frame array
> Js.t

type parse_result = <
  scene: scene;
  dependencies: string array
> Js.t

external mkdirSync : string -> unit = "" [@@bs.val] [@@bs.module "fs"]

let parse_scene target =
  let result =
    target
    |> Node.Fs.readFileAsUtf8Sync
    |> Lexing.from_string
    |> Parser.entry Lexer.token
    |> eval_scene
  in [%bs.obj {
    scene = [%bs.obj {
      label = filename target;
      frames = result.frames
    }];
    dependencies = result.dependencies
  }]

let parse input =
  let baseDir = Node.Path.dirname input in
  let rec inner scenario files =
    match Js.Array.pop files with
    | None -> scenario
    | Some target -> begin
      let result = Node.Path.join [| baseDir; target ^ ".scm" |] |> parse_scene in
      Js.Array.push result##scene scenario |> ignore;
      result##dependencies
      |> Js.Array.filter (fun x -> not (Js.Array.includes x files) && not (Js.Array.some (fun s -> s##label = filename x) scenario))
      |> Js.Array.concat files
      |> inner scenario
    end
  in inner [||] [| filename input |]

let compile outDir input =
  let ast = parse input in
  let result = analyze (Obj.repr ast) in
  let () =
    try mkdirSync outDir
    with _ -> Js.log("output directory already exists: " ^ outDir)
  in
  Array.map (fun s -> s##write outDir) result##scripts
  |> Array.append [| generate outDir result##scenario |]
  |> Js.Promise.all

