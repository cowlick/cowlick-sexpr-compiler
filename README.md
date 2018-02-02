# cowlick-sexpr-compiler

[![Build Status](https://travis-ci.org/cowlick/cowlick-sexpr-compiler.svg?branch=master)](https://travis-ci.org/cowlick/cowlick-sexpr-compiler)

cowlick-sexpr-compilerはS式ベースのスクリプトからcowlick用のJavaScriptを生成するコンパイラです。

## 開発環境

* opam
  * ocamlのインストール
* ocaml
  * `ocamllex`と`ocamlyacc`を使用するため
* Node.js

## ビルド方法

```
npm run build
```

### 注意事項

[opam](./opam)ファイルはTravis CIでテストを実行するために作成したダミーファイルです。
開発時は`opam`ではなく`npm`コマンドを使用してください。
