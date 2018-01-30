let compile input =
  input
  |> Lexing.from_string
  |> Parser.expr Lexer.token
  |> Type.print

let compile_scenario inputDir =
  "first.scm"
  |> Filename.concat inputDir
  |> Node.Fs.readFileAsUtf8Sync
  |> compile

