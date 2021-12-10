
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

  let make input ~read:row : _ t =
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

module Encode = struct
  type 'a t = {
    out: Output.t;
    write: Output.t -> 'a -> unit;
    schema: string;

    pool: Iobuf.Pool.t;
    sync_marker: string; (* len=16 *)

    max_block_count: int;
    mutable closed: bool;
    mutable block_out: Output.t;
    mutable block_q : Iobuf.t Queue.t;
    mutable block_count: int;
  }

  exception Closed

  let write_header_ self =
    let module OH = Obj_container_header in
    let magic = "Obj\x01" in
    Output.write_string_of_len self.out 4 magic;
    (* TODO: also write codec if present *)
    let meta = OH.Str_map.(singleton "avro.schema" self.schema) in
    OH.write self.out meta;
    Output.write_string_of_len self.out 16 self.sync_marker;
    ()

  let make
      ?(max_block_count=50_000) ?(buf_size=16 * 1024) ?pool
      out ~schema ~write : _ t =
    let max_block_count = max 100 (min max_block_count 50_000) in
    let buf_size = max 128 buf_size in
    let pool = match pool with
      | None -> Iobuf.Pool.create ~buf_size ()
      | Some p -> p
    in

    let sync_marker =
      let s = "syncmeup" in
      s ^ s
    in

    (* writing blocks doesn't go directly into [out], we need
       to be able to count bytes (for the block header), and
       optionally to compress the whole content before writing it.
       So we store temporary data in buffers. *)
    let block_out, block_q = Output.of_iobufs pool in

    let self = {
      out; write; schema;
      closed=false; pool;
      max_block_count; sync_marker;
      block_out; block_q; block_count=0;
    } in
    write_header_ self;
    self

  (* write a block to [out] *)
  let[@inline never] flush_block_ self : unit =
    (* TODO: apply codecs first, then compute block size *)

    (* TODO: support other codecs, like deflate.
       In this case we need to produce a new series of buffers
       using Zip_helper (or directly Zlib), and write _that series_ out. *)
    if true then (
      (* codec=null *)
      let block_size =
        let n = ref 0 in
        Queue.iter (fun buf -> n := !n + Iobuf.len buf) self.block_q;
        !n
      in

      (* write block header *)
      Output.write_int self.out self.block_count;
      Output.write_int self.out block_size;
      Queue.iter
        (fun (buf:Iobuf.t) ->
           Output.write_slice self.out buf.b buf.i (Iobuf.len buf))
        self.block_q;
    );

    (* recycle buffers, clear state *)
    Queue.iter (Iobuf.Pool.recycle self.pool) self.block_q;
    Queue.clear self.block_q;
    self.block_count <- 0;

    (* terminate block *)
    Output.write_string_of_len self.out 16 self.sync_marker;
    Output.flush self.out

  (* see, after a push, if we need to flush current block *)
  let[@inline never] post_push_ self =
    if self.block_count >= self.max_block_count then (
      flush_block_ self
    )

  let cur_block_count self = self.block_count
  let flush_block self = if self.block_count > 0 then flush_block_ self

  let[@inline] push self x : unit =
    if self.closed then raise Closed;
    self.write self.block_out x;
    post_push_ self

  let close self =
    if not self.closed then (
      if self.block_count > 0 then flush_block_ self;
      self.closed <- true;
    )
end

