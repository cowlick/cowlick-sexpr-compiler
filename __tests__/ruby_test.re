open Jest;
open Expect;

describe("Ruby", () => {

  let data: array((string, string, array(Ruby.t))) = [|
    ({|ai|}, {|ue|}, [|
      [%bs.obj {
        rb: {|a |},
        rt: {|u |}
      }],
      [%bs.obj {
        rb: {|ai|},
        rt: {|ue|}
      }]
    |]),
    ({j|あい|j}, {j|ｧｨ|j}, [|
      [%bs.obj {
        rb: {j|あ　|j},
        rt: {j|ｧ |j}
      }],
      [%bs.obj {
        rb: {j|あい|j},
        rt: {j|ｧｨ|j}
      }]
    |]),
    ({j|ルビのテスト|j}, {j|テスト|j}, [|
      [%bs.obj {
        rb: {j|ル　　　　　|j},
        rt: {j|テ　　|j}
      }],
      [%bs.obj {
        rb: {j|ルビ　　　　|j},
        rt: {j|テ　　|j}
      }],
      [%bs.obj {
        rb: {j|ルビの　　　|j},
        rt: {j|テス　|j}
      }],
      [%bs.obj {
        rb: {j|ルビのテ　　|j},
        rt: {j|テス　|j}
      }],
      [%bs.obj {
        rb: {j|ルビのテス　|j},
        rt: {j|テスト|j}
      }],
      [%bs.obj {
        rb: {j|ルビのテスト|j},
        rt: {j|テスト|j}
      }]
    |]),
    ({j|abc|j}, {j|abcdef|j}, [|
      [%bs.obj {
        rb: {j|a  |j},
        rt: {j|ab    |j}
      }],
      [%bs.obj {
        rb: {j|ab |j},
        rt: {j|abcd  |j}
      }],
      [%bs.obj {
        rb: {j|abc|j},
        rt: {j|abcdef|j}
      }]
    |])
  |];

  data
  |> Js.Array.forEach (((rb, rt, expected)) => {
    test({j|差分表示に変換できる|j}, () => {
      let actual = Ruby.generate(rb, rt);
      expect(actual) |> toEqual(expected)
    })
  });
});

