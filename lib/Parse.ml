open Printf

let print_error lexer =
  let position = Lexing.lexeme_start_p lexer in
  let line = position.Lexing.pos_lnum
  and char = position.Lexing.pos_cnum - position.Lexing.pos_bol in
  sprintf "at line %d, column %d: syntax error." line char

let input_channel name =
  let input = if name <> "-" then open_in name else stdin in
  let close = if name <> "-" then (fun () -> close_in input) else (fun () -> ()) in
  let lexer = Lexing.from_channel input in
  lexer, close

module type LS = sig
  module P : sig type token end
  val token : Lexing.lexbuf -> P.token
  exception Error of string
end

module TableBased
  (I : MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE)
  (L : LS with type P.token = I.token)
  (PM : sig val message : int -> string end) =
struct
(*  open Lexing*)

(* A short name for the incremental parser API. *)
(*module I = Parser.MenhirInterpreter
module IQ = ParserQCIR.MenhirInterpreter*)
module G = MenhirLib.General

exception ParserError of string

(* Adapted from F.Pottier code in CompCert. *)
(* Hmm... The parser is in its initial state. Its number is usually 0. This is a BIG HACK. TEMPORARY *)
let stack = function
  | I.HandlingError env -> Lazy.force (I.stack env)
  | _ -> assert false (* this cannot happen, F. Pottier promises *)
let state checkpoint : int = match stack checkpoint with
  | G.Nil -> 0
  | G.Cons (I.Element (s, _, _, _), _) -> I.number s

let succeed v = v
let fail lexbuf checkpoint =
  let error_nb = state checkpoint in
  let herror = PM.message error_nb in
  (*let herror = ParsingErrors.message (state checkpoint) in*)
  let position = print_error lexbuf in
  let message = sprintf "%s %s (error %d)" position herror error_nb in
  raise (ParserError message)

let loop lexbuf result =
  let supplier = I.lexer_lexbuf_to_supplier L.token lexbuf in
  I.loop_handle succeed (fail lexbuf) supplier result

let from_file entry name =
  let (lexer, close) = input_channel name in
  let reduc =
    try loop lexer (entry lexer.Lexing.lex_curr_p) with
    | L.Error msg -> close (); invalid_arg (sprintf "Lexer error. In file %s, %s" name msg)
    | ParserError msg -> close (); invalid_arg (sprintf "Parser error. In file %s, %s" name msg)
    | exn -> close (); eprintf "The error message file might be outdated.\nIn file %s, %s\n" name (print_error lexer); raise exn in
  close ();
  reduc

(*let loop_qcir lexbuf result =
  let supplier = IQ.lexer_lexbuf_to_supplier LexerQCIR.token lexbuf in
  IQ.loop_handle succeed (fail ParserMessagesQCIR.message lexbuf) supplier result

let qcir_from_file () name =
  let (lexer, close) = input_channel name in
  let reduc =
    try loop_qcir lexer (ParserQCIR.Incremental.file lexer.Lexing.lex_curr_p) with
    | LexerQCIR.Error msg -> close (); invalid_arg (sprintf "Lexer error. In file %s, %s" name msg)
    | ParserError msg -> close (); invalid_arg (sprintf "Parser error. In file %s, %s" name msg)
    | exn -> close (); eprintf "The error message file might be outdated.\nIn file %s, %s\n" name (print_error lexer); raise exn in
  close ();
  reduc*)
end

module DL = TableBased (Parser.MenhirInterpreter) (Lexer) (ParserMessages)
let from_file () = DL.from_file Parser.Incremental.file

module QC = TableBased (ParserQCIR.MenhirInterpreter) (LexerQCIR) (ParserMessagesQCIR)

let qcir_from_file () = QC.from_file ParserQCIR.Incremental.file
