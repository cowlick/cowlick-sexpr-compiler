open Jest;
open Expect;

describe("SExprParser", () => {

  describe({j|正しい構文が処理できる|j}, () => {

    let path = "__tests__/fixture/sexpr/valid/";

    Glob.sync({j|$(path)**/*.scm|j})
    |> Js.Array.forEach ((filePath) => {
      let baseName =
        filePath
        |> Js.String.substrAtMost(~from = 0, ~length = String.length(filePath) - String.length("/content.scm"))
        |> Js.String.substr(~from = String.length(path));
      test(filePath, () => {
        let actual = Compiler.parse_scene(Node.Path.dirname(filePath), Node.Path.basename(filePath));
        let frames =
          actual.scene##frames
          |> Obj.magic
          |> Js.Json.stringify
          |> Js.Json.parseExn;
        let expected =
          {j|$(path)$(baseName)/content.ast|j}
          |> Node.Fs.readFileAsUtf8Sync
          |> Js.Json.parseExn;
        expect(frames) |> toEqual(expected)
      })
    })
  });
});

