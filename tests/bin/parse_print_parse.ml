open Avro_compiler_lib

let unwrap_ = function
  | Ok x -> x
  | Error e -> failwith e

let () =
  let file = Sys.argv.(1) in
  Printf.printf "file: %S\n" file;

  let schema = Schema.parse_file file |> unwrap_ in
  Printf.printf "parsed schema\n";
  let s = Schema.to_string schema in
  Printf.printf "re-encoded schema:\n%s\n" s;
  let _schema2 = Schema.parse_string s |> unwrap_ in
  Printf.printf "parsed printed schema\n";
  ()
