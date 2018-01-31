let parse_scene target =
  target
  |> Node.Fs.readFileAsUtf8Sync
  |> Lexing.from_string
  |> Parser.expr Lexer.token
  |> Eval.eval_scene

let parse_scenario baseDir =
  "first.scm"
  |> Filename.concat baseDir
  |> parse_scene

let compile inputDir =
  parse_scenario inputDir

