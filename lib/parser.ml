open Ast
open Grammar

(* exception ParseError of string

let parse (src : string) : program =
  src |> Lexing.from_string |> Grammar.prog Lexer.tok *)

include Nice_parser.Make(struct
  type result = program
  type token = Grammar.token
  exception ParseError = Grammar.Error
  let parse = Grammar.prog
  include Lexer
end)

let () = pp_exceptions ()

let parse = parse_string