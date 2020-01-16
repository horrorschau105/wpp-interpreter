{
open Lexing
open Parser
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let nat = ['0'-'9']+
let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '\'']*

rule read =
  parse
  | white {read lexbuf}
  | newline {new_line lexbuf; read lexbuf}
  | nat {NUM (int_of_string (Lexing.lexeme lexbuf))}
  | "in" {IN}
  | "ifz" {IFZ}
  | "then" {THEN}
  | "else" {ELSE}
  | "while" {WHILE}
  | "skip" {SKIP}
  | "vars" {VARS}
  | "do" {DO}
  | "tuple" {TUPLE}
  | "Tuple" {TTUPLE}
  | "function" {FUNC}
  | "return" {RETURN}
  | "int" {TNAT}
  | "Sum" {TSUM}
  | "case" {CASE}
  | "type" {TYPE}
  | "Ptr" {PTR}
  | "new" {NEW}
  | "|" {PIPE}
  | "->" {OF}
  | "." {DOT}
  | ";" {SCOLON}
  | ":=" {INITIALIZE}
  | "=" {ASSIGN}
  | "-" {MINUS}
  | "*"  {PROD}
  | "+"  {PLUS}
  | "," {COMMA}
  | "(" {LPAR}
  | ")" {RPAR}
  | "{" {BLOCKBEG}
  | "}" {BLOCKEND}
  | "[" {LINDEXER}
  | "]" {RINDEXER}
  | id {ID (Lexing.lexeme lexbuf)}
  | _ {failwith ("Unexpected char: " ^ Lexing.lexeme lexbuf)}
  | eof {EOF}
