
let spf = Printf.sprintf

let codecs = ref [
    "null", (fun s->s);
    (* TODO
    "deflate", (fun s -> Zip_helper.
       *)
]

let register_decompression_codec name ~decompress : unit =
  if List.mem_assoc name !codecs then failwith "codec already exists";
  codecs := (name, decompress) :: !codecs


module Decode = struct
  type 'a t = {
    input: Input.t;
    row: (Input.t -> 'a);
    mutable is_done: bool;

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
    let header = Obj_container_header.read self.input in
    (* TODO: read avro.schema, and avro.codec *)
    assert false


  let make input row : _ t =
    let self = {
      input; row; is_done=false;
      block_input=input;
      block_remaining_count=0;
    } in
    let bu
    (* TODO:
    - read header *)
    assert false

  let cur_block_remaining_count self = self.block_remaining_count

  let next self =
end
