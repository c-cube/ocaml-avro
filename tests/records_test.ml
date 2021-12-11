
module R = Records

let () =
  let buf = Buffer.create 125 in
  let out = Avro.Output.of_buffer buf in

  let n = (try Sys.getenv "N" |> int_of_string with _ -> 50_000) in

  Avro.Obj_container_file.Encode.(
    let enc = make out ~max_block_count:500 ~write:R.write ~schema:R.schema in
    for i=1 to n do
      push enc {R.a=Int64.of_int i; b="foo_"^string_of_int i}
    done;
    close enc
  );
  let str = Buffer.contents buf in

  Printf.printf "output for %d items: size=%d bytes\n" n (String.length str);

  begin match Sys.getenv_opt "OUT" with
     | None -> ()
     | Some f ->
       Printf.printf "write to file %S\n" f;
       let oc = open_out f in
       output_string oc str;
       close_out oc
  end;

  (* now let's read *)
  Printf.printf "reading back…\n%!";

  let rows = Avro.Obj_container_file.Decode.(
      let dec = make (Avro.Input.of_string str) ~read:R.read in
      to_list dec)
  in

  Printf.printf "read back %d rows\n" (List.length rows);
  ()

