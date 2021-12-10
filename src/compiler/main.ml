open Avro_compiler_lib

let spf = Printf.sprintf

let debug_ = ref false
let out_file_ = ref ""
let internal = ref false

let compile (file:string) : unit =
  let schema = match Schema.parse_file file with
    | Ok s -> s
    | Error e -> Printf.printf "cannot compile '%s':\n%s" file e; exit 1
  in
  if !debug_ then Format.printf "schema: %a@." Schema.pp schema;
  let code = Codegen.gen ~internal:!internal schema |> Codegen.Code.to_string in
  if !out_file_ = "" then print_string code
  else (
    let oc = open_out !out_file_ in
    output_string oc code; flush oc; close_out oc
  )

let () =
  let files = ref [] in
  let args = [
    "-d", Arg.Set debug_, " debug mode";
    "-o", Arg.Set_string out_file_, " output file";
    "--internal", Arg.Set internal, " internal mode (will not `open Avro`)";
  ] |> Arg.align in
  Arg.parse args (fun f -> files := f :: !files) "avro-compiler file [opt*]";

  match !files with
  | [] -> failwith "please provide an input file"
  | [f] -> compile f
  | _ -> failwith "please provide only one input file"
