open Type
open Printf

exception Translate_error of string
let error message = raise (Translate_error message)

let rec translate = function
| Number v -> number_ast v
| Bool  v -> bool_ast v
| String v -> str_ast v
| expr -> error ("not implemented: %s" ^ (to_str expr))

and literal value raw =
  [%bs.obj {
    _type = "Literal";
    value = value;
    raw = raw
  }]

and number_ast value =
  literal (Obj.repr value) (sprintf "%f" value)

and bool_ast value =
  literal (Obj.repr value) (sprintf "%b" value)

and str_ast str =
  literal (Obj.repr str) (sprintf "\"%s\"" str)

let assign typ name expr =
  [%bs.obj {
    _type = "Program";
    body = [|
      {
        _type = "ExpressionStatement";
        expression = {
          _type = "AssignmentExpression";
          operator = "=";
          left = {
            _type = "MemberExpression";
            _object = {
              _type = "MemberExpression";
              _object = {
                _type = "Identifier";
                name = "variables"
              };
              property = {
                _type = "Identifier";
                name = typ
              };
              computed = Js.false_
            };
            property = {
              _type = "Identifier";
              name = name
            };
            computed = Js.false_
          };
          right = translate expr
        }
      }
    |];
    sourceType = "script"
  }]

