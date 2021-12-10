
let spf = Printf.sprintf

let codecs = ref [
    "null", None;
    "deflate", Some (fun str ->
        match Zip_helper.decompress ~header:false str with
        | Error (`Zlib e) ->
          failwith (Format.asprintf "Input: cannot read compressed string:@ %a"
                      Zip_helper.pp_error e)
        | Ok str' -> str'
      );
]

let register_decompression_codec name ~decompress : unit =
  if List.mem_assoc name !codecs then failwith "codec already exists";
  codecs := (name, Some decompress) :: !codecs


module Decode = struct
  type 'a t = {
    input: Input.t;
    row: (Input.t -> 'a);
    mutable is_done: bool;

    mutable codec: (string -> string) option; (* decompression *)

    mutable block_input: Input.t;
    (* an input specialized for the current block. Might be the
       same as {!input}. *)

    mutable block_remaining_count: int;
    (** How many items in the current block. *)
  }

  let read_header_ self =
    (* read magic number *)
    let magic = Bytes.make 4 '\x00' in
    Input.read_exact self.input magic 0 4;
    let magic = Bytes.unsafe_to_string magic in
    if magic <> "Obj\x01" then (
      failwith (spf "cannot decode file container: expected 'Obj\x01' header, got %S" magic);
    );

    (* read header *)
    let module OH = Obj_container_header in
    let header = OH.read self.input in
    let codec = match OH.Str_map.find_opt "avro.codec" header with
      | None -> None
      | Some name ->
        begin match List.assoc_opt name !codecs with
          | Some c -> c
          | None -> failwith (spf "unknown codec %S" name)
        end
    in
    self.codec <- codec

  let make input row : _ t =
    let self = {
      input; row; is_done=false;
      codec=None;
      block_input=input;
      block_remaining_count=0;
    } in
    read_header_ self;
    self

  let cur_block_remaining_count self = self.block_remaining_count

  let next self =
    None (* TODO *)

  let rec to_seq self () =
    match next self with
    | None -> Seq.Nil
    | Some x -> Seq.Cons (x, to_seq self)

  let rec iter self ~f =
    match next self with
    | None -> ()
    | Some x -> f x; iter self ~f

  let fold self ~f ~init =
    let r = ref init in
    iter self ~f:(fun x -> r := f !r x);
    !r

  let to_list self = List.of_seq @@ to_seq self
  let to_array self = Array.of_seq @@ to_seq self
end
