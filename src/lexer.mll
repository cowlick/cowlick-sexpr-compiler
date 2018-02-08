{

open Printf
open Parser

exception Syntax_error of string
let error message = raise (Syntax_error message)

let add_utf8 buf code =
  if code <= 0x7f then Buffer.add_char buf (Char.chr code)
  else if code <= 0x7ff then begin
    Buffer.add_char buf (Char.chr (0b11000000 lor ((code lsr 6) land 0x3f)));
    Buffer.add_char buf (Char.chr (0b10000000 lor (code land 0x3f)))
  end else begin
    Buffer.add_char buf (Char.chr (0b11100000 lor ((code lsr 12) land 0x3f)));
    Buffer.add_char buf (Char.chr (0b10000000 lor ((code lsr 6) land 0x3f)));
    Buffer.add_char buf (Char.chr (0b10000000 lor (code land 0x3f)))
  end

type state =
  | Code
  | InterpolatedString
let stack = [||]
let state = ref Code

let state_to_str = function
| Code -> "code"
| InterpolatedString -> "string interpolation"

}

let newline = '\n' | "\r\n"
let hex = ['0'-'9'] | ['a'-'f'] | ['A'-'F']

rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | ';' { line_comment lexbuf }
  | ['0'-'9']+ { NUMBER (float_of_string(Lexing.lexeme lexbuf)) }
  | ['0'-'9']*'.'['0'-'9']+ { NUMBER (float_of_string(Lexing.lexeme lexbuf)) }
  | "#t" { TRUE }
  | "#f" { FALSE }
  | '#' '"' {
    match !state with
    | Code ->
      state := InterpolatedString;
      string (Buffer.create 100) lexbuf
    | s -> error (sprintf "[%s] can not nest string interpolation." (state_to_str s))
  }
  | '"' {
    match !state with
    | Code -> string (Buffer.create 100) lexbuf
    | s -> error (sprintf "[%s] can not include string." (state_to_str s))
  }
  | '|' {
    match Js.Array.pop stack with
    | Some InterpolatedString ->
      state := InterpolatedString;
      string (Buffer.create 100) lexbuf
    | _ -> error (sprintf "[%s] invalid token: |" (state_to_str (!state)))
  }
  | [^'(' ')' '0' - '9' ' ' '\t' '\n' '.' '\'' '"' '|' '#'][^' ' '\t' '\n' '(' ')']* {
    SYMBOL(Lexing.lexeme lexbuf)
  }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '.' { DOT }
  | eof { EOF }

and string buf = parse
  | '"' {
    match !state with
    | InterpolatedString ->
      state := Code;
      INTERPOLATED_STRING_END(Buffer.contents buf)
    | Code -> STRING(Buffer.contents buf)
  }
  | "~|" {
    match !state with
    | InterpolatedString -> begin
      state := Code;
      Js.Array.push InterpolatedString stack |> ignore;
      INTERPOLATED_STRING(Buffer.contents buf)
    end
    | Code -> begin
      Buffer.add_string buf "~|";
      string buf lexbuf
    end
  }
  | "~~" {
    begin match !state with
    | InterpolatedString -> Buffer.add_char buf '~'
    | Code -> Buffer.add_string buf "~~"
    end;
    string buf lexbuf
  }
  | '\\' '/' { Buffer.add_char buf '/'; string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; string buf lexbuf }
  | '\\' 'b' { Buffer.add_char buf '\b'; string buf lexbuf }
  | '\\' 'f' { Buffer.add_char buf '\012'; string buf lexbuf }
  | '\\' 'n' { Buffer.add_char buf '\n'; string buf lexbuf }
  | '\\' 'r' { Buffer.add_char buf '\r'; string buf lexbuf }
  | '\\' 't' { Buffer.add_char buf '\t'; string buf lexbuf }
  | '\\' 'u' hex hex hex hex {
    let code = int_of_string ("0x" ^ (String.sub (Lexing.lexeme lexbuf) 2 4)) in
    add_utf8 buf code;
    string buf lexbuf
  }
  | [^ '"' '\\' '~']+ {
    Buffer.add_string buf (Lexing.lexeme lexbuf);
    string buf lexbuf
  }
  | _ { error ("Illegal character in utf8 string: " ^ Lexing.lexeme lexbuf) }

and line_comment = parse
  | newline { token lexbuf }
  | _ { line_comment lexbuf }

{}

