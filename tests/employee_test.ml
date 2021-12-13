
module E = Employee
let spf = Printf.sprintf

type decode_into =
  | Seq
  | List_of_arrays

let string_of_dec_into = function
  | Seq -> "seq"
  | List_of_arrays -> "list-of-arrays"

let seq_len seq =
  let rec loop n seq = match seq() with
    | Seq.Nil -> n
    | Seq.Cons (_, tl) -> loop (n+1) tl
  in
  loop 0 seq

let encode_to_out ~codec_name ~n (out:Avro.Output.t) =
  let open Avro in
  let open Obj_container_file in
  let codec =
    try Codec.find_by_name_exn codec_name
    with Not_found -> failwith "unknown codec"
  in
  let enc =
    Encode.make out ~max_block_count:500 ~codec ~write:E.write ~schema:E.schema in

  let langs = [|E.Cpp; E.Ocaml; E.Java; E.Python|] in
  for i=1 to n do
    let e1 = {
      E.firstname=spf"hacker_%d" i;
      lastname=spf"man_%d"i;
      friendliness=float (i mod 100) *. 100.;
      age=(i mod 40) + 18;
      job=E.C_programmer {
          E.language=langs.(i mod Array.length langs)
        };
    }
    and e2 = {
      E.firstname=spf"dil_%d" i;
      lastname=spf"bert_%d"i;
      friendliness= 0.1;
      age=(i mod 35) + 30;
      job=E.C_RH {E.hair_spikiness="++++"};
    } in
    Encode.push enc e1;
    Encode.push enc e2;
  done;
  Encode.close enc

(* test by encoding/decoding into a string *)
let test_str ~codec_name ~n () : string =
  let buf = Buffer.create 125 in
  let out = Avro.Output.of_buffer buf in
  encode_to_out ~codec_name ~n out;
  let str = Buffer.contents buf in

  Printf.printf "output for %d items: size=%d bytes (codec %S)\n"
    (2*n) (String.length str) codec_name;
  str

let test_file ~codec_name ~n file : unit =
  Avro.Output.with_file file @@ fun out ->
  encode_to_out ~codec_name ~n out;
  ()

let decode_from ~decode_into (input:Avro.Input.t) : int =
  let open Avro.Obj_container_file.Decode in
  let dec = make input ~read:E.read in
  match decode_into with
  | Seq ->
    let seq = to_seq dec in
    seq_len seq
  | List_of_arrays ->
    let l = to_array_list dec in
    List.fold_left (fun n a -> n + Array.length a) 0 l

let test_str_read ~decode_into (str:string ) : unit =
  Printf.printf "reading back from string into %s…\n%!" (string_of_dec_into decode_into);
  let n = decode_from ~decode_into (Avro.Input.of_string str) in
  Printf.printf "read back %d rows\n%!" n;
  ()

let test_file_read ~decode_into file : unit =
  Printf.printf "reading back from file %S into %s…\n%!"
    file (string_of_dec_into decode_into);
  Avro.Input.with_file file @@ fun input ->
  let n = decode_from ~decode_into input in
  Printf.printf "read back %d rows\n%!" n;
  ()

let () =
  Printexc.record_backtrace true;
  let out = ref "" in
  let codec_name = ref "null" in
  let n = ref 50_000 in
  let dec_into = ref Seq in

  let opts = [
    "-o", Arg.Set_string out, " output file";
    "-n", Arg.Set_int n, " number of items";
    "--codec", Arg.Set_string codec_name, " compression codec";
    "--seq", Arg.Unit (fun()->dec_into := Seq), " decode as seq";
    "--list-of-array", Arg.Unit (fun()->dec_into := List_of_arrays), " decode as list of arrays";
  ] |> Arg.align in
  Arg.parse opts (fun _ -> failwith "no positional arg") "";

  let codec_name = !codec_name in
  let n = !n in
  let decode_into = !dec_into in
  begin match !out with
    | "" ->
      let str = test_str ~codec_name ~n () in
      test_str_read ~decode_into str;
    | file ->
      Printf.printf "write to file %S\n%!" file;
      test_file ~codec_name ~n file;
      test_file_read ~decode_into file;
  end;
  ()

