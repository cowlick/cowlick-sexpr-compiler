open Jest;
open Expect;
open Type;

describe("Js_ast", () => {

  [|
    (Bool(true), {|{
  "type": "Literal",
  "value": true,
  "raw": "true"
}|}),
    (Cons(Symbol("assert"), Cons(Bool(true), Nil)), {|{
  "type": "CallExpression",
  "callee": {
    "type": "Identifier",
    "name": "assert"
  },
  "arguments": [
    {
      "type": "Literal",
      "value": true,
      "raw": "true"
    }
  ]
}|})
  |]
  |> Js.Array.forEach (((sexpr, ast)) => {
    let name = to_str(sexpr);
    test({j|S式をJavaScript ASTに変換できる $name|j}, () => {
      let expected = Js.Json.parseExn(ast);
      expect(Js_ast.translate(sexpr) |> Obj.magic) |> toEqual(expected)
    })
  });
});

