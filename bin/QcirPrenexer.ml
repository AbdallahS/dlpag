open Printf
open Dlpag

let get () =
  let speclist = [] in
  let usage_msg = "QCIR Prenexer. " in
  let input_names = ref [] in
  let add_name s = input_names := s :: !input_names in
  Arg.parse speclist add_name usage_msg;
  match !input_names with
  | n :: [] -> n
  | _ :: _ :: _ | [] -> failwith "Wrong number of arguments. Usage: qcirprenexer file"

let start () =
  let f = get () in
  let parsed = Parse.qcir_from_file () f in
  let qcir = QCIR.sanitize_names parsed in
  printf "%s\n" (QCIR.Print.file qcir);
  ()

let _ = start ()
