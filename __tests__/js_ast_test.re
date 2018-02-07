open Jest;
open Expect;
open Type;

let unary = (name) => (Cons(Symbol(name), Cons(Number(1.), Nil)), {j|{
  "type": "UnaryExpression",
  "operator": "$(name)",
  "argument": {
    "type": "Literal",
    "value": 1,
    "raw": "1.000000"
  }
}|j});

let arg1 = (name) => (Cons(Symbol(name), Cons(Number(1.), Nil)), {j|{
  "type": "BinaryExpression",
  "operator": "$(name)",
  "left": {
    "type": "Literal",
    "value": 1,
    "raw": "1.000000"
  },
  "right": {
    "type": "Literal",
    "value": 1,
    "raw": "1.000000"
  }
}|j});

let binary = (name) => (Cons(Symbol(name), Cons(Number(0.), Cons(Number(1.), Nil))), {j|{
  "type": "BinaryExpression",
  "operator": "$(name)",
  "left": {
    "type": "Literal",
    "value": 0,
    "raw": "0.000000"
  },
  "right": {
    "type": "Literal",
    "value": 1,
    "raw": "1.000000"
  }
}|j});

let arg3 = (name) => (Cons(Symbol(name), Cons(Number(5.), Cons(Number(2.), Cons(Number(1.), Nil)))), {j|{
  "type": "BinaryExpression",
  "operator": "$(name)",
  "left": {
    "type": "BinaryExpression",
    "operator": "$(name)",
    "left": {
      "type": "Literal",
      "value": 5,
      "raw": "5.000000"
    },
    "right": {
      "type": "Literal",
      "value": 2,
      "raw": "2.000000"
    }
  },
  "right": {
    "type": "Literal",
    "value": 1,
    "raw": "1.000000"
  }
}|j});

let arg4 = (name) => (Cons(Symbol(name), Cons(Number(5.), Cons(Number(2.), Cons(Number(1.), Cons(Number(1.), Nil))))), {j|{
  "type": "BinaryExpression",
  "operator": "$(name)",
  "left": {
    "type": "BinaryExpression",
    "operator": "$(name)",
    "left": {
      "type": "BinaryExpression",
      "operator": "$(name)",
      "left": {
        "type": "Literal",
        "value": 5,
        "raw": "5.000000"
      },
      "right": {
        "type": "Literal",
        "value": 2,
        "raw": "2.000000"
      }
    },
    "right": {
      "type": "Literal",
      "value": 1,
      "raw": "1.000000"
    }
  },
  "right": {
    "type": "Literal",
    "value": 1,
    "raw": "1.000000"
  }
}|j});

describe("Js_ast", () => {

  [|
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
}|}),
    unary("+"),
    unary("-"),
    binary("+"),
    binary("-"),
    binary("*"),
    binary("/"),
    arg1("*"),
    arg1("/"),
    (Cons(Symbol("+"), Nil), {j|{
  "type": "Literal",
  "value": 0,
  "raw": "0.000000"
}|j}),
    (Cons(Symbol("*"), Nil), {j|{
  "type": "Literal",
  "value": 1,
  "raw": "1.000000"
}|j}),
    arg3("-"),
    arg4("-")
  |]
  |> Js.Array.forEach (((sexpr, ast)) => {
    let name = to_str(sexpr);
    test({j|S式をJavaScript ASTに変換できる $name|j}, () => {
      let expected = Js.Json.parseExn(ast);
      expect(Js_ast.translate(sexpr) |> Obj.magic) |> toEqual(expected)
    })
  });
});

