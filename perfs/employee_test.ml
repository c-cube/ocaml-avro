
module E = Employee
let spf = Printf.sprintf

let () =
  Tracy.enable();
  let buf = Buffer.create 125 in
  let out = Avro.Output.of_buffer buf in

  let codec_name = (try Sys.getenv "CODEC" with _ -> "deflate") in
  let n = (try Sys.getenv "N" |> int_of_string with _ -> 1_000_000) in

  Avro.Obj_container_file.(Encode.(
      Tracy.with_ ~file:__FILE__ ~line:__LINE__ ~name:"enc" () @@ fun _sp ->
      let codec =
        try Codec.find_by_name_exn codec_name
        with Not_found -> failwith "unknown codec"
      in
      let enc = make out ~max_block_count:500 ~codec ~write:E.write ~schema:E.schema in

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
      close enc
    ));
  let str = Buffer.contents buf in

  Printf.printf "output for %d items: size=%d bytes (codec %S)\n"
    (2*n) (String.length str) codec_name;

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
      Tracy.with_ ~file:__FILE__ ~line:__LINE__ ~name:"dec" () @@ fun _sp ->
      let dec = make (Avro.Input.of_string str) ~read:E.read in
      to_list dec)
  in

  Printf.printf "read back %d rows\n" (List.length rows);
  ()

