{

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
}

let hex = ['0'-'9'] | ['a'-'f'] | ['A'-'F']

rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | ['0'-'9']+ { NUMBER (float_of_string(Lexing.lexeme lexbuf)) }
  | ['0'-'9']*'.'['0'-'9']+ { NUMBER (float_of_string(Lexing.lexeme lexbuf)) }
  | "#t" { TRUE }
  | "#f" { FALSE }
  | "if" { IF }
  | '"' { string (Buffer.create 100) lexbuf }
  | [^'(' ')' '0' - '9' ' ' '\t' '\n' '.' '\'' '"'][^' ' '\t' '\n' '(' ')']* {
    SYMBOL(Lexing.lexeme lexbuf)
  }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '.' { DOT }
  | eof { EOF }

and string buf = parse
  | '"' { STRING(Buffer.contents buf) }
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
  | [^ '"' '\\']+ {
    Buffer.add_string buf (Lexing.lexeme lexbuf);
    string buf lexbuf
  }
  | _ { error ("Illegal character in utf8 string: " ^ Lexing.lexeme lexbuf) }

{}

