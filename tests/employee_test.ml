
module E = Employee
let spf = Printf.sprintf

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

let decode_from (input:Avro.Input.t) : int =
  let open Avro.Obj_container_file.Decode in
  let dec = make input ~read:E.read in
  let seq = to_seq dec in
  seq_len seq

let test_str_read (str:string ) : unit =
  Printf.printf "reading back from string…\n%!";
  let n = decode_from (Avro.Input.of_string str) in
  Printf.printf "read back %d rows\n" n;
  ()

let test_file_read file : unit =
  Printf.printf "reading back from file %S…\n%!" file;
  Avro.Input.with_file file @@ fun input ->
  let n = decode_from input in
  Printf.printf "read back %d rows\n" n;
  ()

let () =
  Printexc.record_backtrace true;

  let codec_name = (try Sys.getenv "CODEC" with _ -> "null") in
  let n = (try Sys.getenv "N" |> int_of_string with _ -> 50_000) in

  begin match Sys.getenv_opt "OUT" with
    | None ->
      let str = test_str ~codec_name ~n () in
      test_str_read str;
    | Some file ->
      Printf.printf "write to file %S\n" file;
      test_file ~codec_name ~n file;
      test_file_read file;
  end;
  ()

