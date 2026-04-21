(** COMP 324 Project 3:  implementation of an imperative language with dynamic
 * security enforcement.
 *
 * `dynsec driver program.
 *
 * @author N. Danner
 *)

(* ****************************************
 * Usage
 * ****************************************
 *)

let usage = {|
  dynsec parseexp e:  parse expresion e
  dynsec parsestm s:  parse statement s
  dynsec parsepgm f:  parse program in file f
  dynsec exec f:  execute program f (path to file)
|}

exception Usage_error of string

module Cli = struct

  module Lexer = Dynsec.Lexer
  module Parser = Dynsec.Parser
  module Ast = Dynsec.Ast
  module Interp = Dynsec.Interp

  exception Parse_error of string

  type parse_cmd_t = Expr | Stm | Pgm

  let parse_and_show (cmd : parse_cmd_t) (s : string) : unit =

      let parse_and_show' parser shower s =
        let lexbuf = Lexing.from_string s in
        try
          let exp = parser Lexer.read_token lexbuf in
          print_endline (shower exp)
        with
        | Parser.Error ->
          let pos = Lexing.lexeme_start_p lexbuf in
          raise @@ Parse_error (
            Printf.sprintf
              "Parser error near line %d, character %d.\n"
              pos.pos_lnum
              (pos.pos_cnum - pos.pos_bol)
          )
      in

      match cmd with
      | Expr -> parse_and_show' Parser.terminated_exp Ast.Expr.show s
      | Stm -> parse_and_show' Parser.terminated_stm Ast.Stm.show s
      | Pgm -> 
        In_channel.with_open_text s (fun inch ->
          parse_and_show' Parser.terminated_pgm Ast.Prog.show
          (In_channel.input_all inch)
        )

  let exec (s : string) : unit =
    In_channel.with_open_text s (fun inch ->
      let lexbuf = Lexing.from_channel inch in
      try
        let p = Parser.terminated_pgm Lexer.read_token lexbuf in
        Interp.exec p
      with
      | Parser.Error ->
        let pos = Lexing.lexeme_start_p lexbuf in
        raise @@ Parse_error (
          Printf.sprintf
            "Parser error near line %d, character %d.\n"
            pos.pos_lnum
            (pos.pos_cnum - pos.pos_bol)
        )
    )
end

let () =
  try
    let args = ref [] in
    Arg.parse [] (fun a -> args := a :: !args) usage ;

    let (command, arg) =
      match List.rev !args with
      | [command; arg] -> (command, arg)
      | _ -> raise @@ Usage_error "wrong number of command line arguments"
    in

    let cmd =
      match command with
      | "parseexp" -> Cli.parse_and_show Expr
      | "parsestm" -> Cli.parse_and_show Stm
      | "parsepgm" -> Cli.parse_and_show Pgm
      | "exec" -> Cli.exec
      | _ -> raise @@ Usage_error "bad command"
    in

    try
      cmd arg
    with
    | Cli.Parse_error msg -> print_endline ("Parse error: " ^ msg)
    | Dynsec.Interp.MultipleDeclaration x ->
      Printf.printf
        "Error: variable '%s' multiply declared.\n"
        (Dynsec.Ast.Id.show x)
    | Dynsec.Interp.UnboundVariable x ->
      Printf.printf
        "Error: variable '%s' used by not declared.\n"
        (Dynsec.Ast.Id.show x)
    | Dynsec.Interp.UndefinedFunction f ->
      Printf.printf
        "Error: function '%s' called but not defined.\n"
        (Dynsec.Ast.Id.show f)
    | Dynsec.Interp.NoReturn f ->
      Printf.printf
        "Error: function '%s' terminated without executing return.\n"
        (Dynsec.Ast.Id.show f)

  with
  | Usage_error msg ->
    Printf.printf
      "Bad command line usage:  %s"
      msg ;
    print_endline "" ;
    print_endline usage

