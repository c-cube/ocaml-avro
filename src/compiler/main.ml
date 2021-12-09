
let spf = Printf.sprintf

let debug_ = ref false
let out_file_ = ref ""

let compile (file:string) : unit =
  let schema = match Schema.parse_file file with
    | Ok s -> s
    | Error e -> failwith (spf "cannot compile '%s':\n%s" file e)
  in
  Format.printf "schema: %a@." Schema.pp schema;
  assert false

let () =
  let files = ref [] in
  let args = [
    "-d", Arg.Set debug_, " debug mode";
    "-o", Arg.Set_string out_file_, " output file";
  ] |> Arg.align in
  Arg.parse args (fun f -> files := f :: !files) "avro-compiler file [opt*]";

  match !files with
  | [] -> failwith "please provide an input file"
  | [f] -> compile f
  | _ -> failwith "please provide only one input file"
