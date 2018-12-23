open Commandpost

type root_options = <
  out: string;
  plugins: string array;
> Js.t

type root_args = <
  entry: string;
> Js.t

let root: (root_options, root_args) Command.t =
  create "cowlick-sexpr-compiler <entry>"
  |. Command.version ~version:"" ~flags:"-v, --version"
  |. Command.help "-h, --help" "print this help"
  |. Command.option ~flags:"-o, --out" ~description:"output directory" ~default_value:"script" ()
  |. Command.action (fun opts args _ ->
    let current = Sys.getcwd () in
    let out = Node.Path.join [|current; opts##out|] in
    let plugins =
      opts##plugins
      |> Array.map (fun p -> Analyzer.plugin (Node.Path.resolve current p)) in
    args##entry
    |> Filename.concat current
    |> Compiler.compile out plugins
  );;

root
|. exec Sys.argv
|> Js.Promise.catch (fun e ->
  Js.log(e);
  Js.Promise.resolve ()
);;
