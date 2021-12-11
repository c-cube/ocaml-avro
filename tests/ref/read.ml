
module OF = Avro.Obj_container_file

let ppuser out (u:User.t) : unit =
  Format.fprintf out "{@[name: %s, favorite_number: %a,@ favorite color: %a@]}"
    u.name
    (Format.pp_print_option Format.pp_print_int) u.favorite_number
    (Format.pp_print_option Format.pp_print_string) u.favorite_color

let () =
  Printexc.record_backtrace true;
  let file = Sys.argv.(1) in
  Printf.printf "read from %S\n" file;

  Avro.Input.with_file file @@ fun input ->
  let dec = OF.Decode.make input ~read:User.read in
  (*
  let l = OF.Decode.to_list dec in
  Format.printf "read %d users@." (List.length l);
  List.iter (Format.printf "user: %a@." ppuser) l;
     *)

  let seq = OF.Decode.to_seq dec in
  Seq.iter (Format.printf "user: %a@." ppuser) seq;

  ()
