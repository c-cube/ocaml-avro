
let spf = Printf.sprintf

module Codec = struct
  type t = {
    name: string;
    compress: string -> string;
    decompress: string -> string;
  }

  let null = {
    name="null"; compress=(fun _->assert false); decompress=(fun _ -> assert false);
  }

  let[@inline] name self = self.name
  let[@inline] is_null_ self = self == null

  let deflate = {
    name="deflate";
    decompress=(fun str ->
        match Zip_helper.decompress ~header:false str with
        | Error (`Zlib e) ->
          failwith (Format.asprintf "Input: cannot read compressed string:@ %a"
                      Zip_helper.pp_error e)
        | Ok str' -> str'
      );
    compress=(fun str ->
        Zip_helper.compress ~header:false str
      );
  }

  let all_ = ref [ null; deflate ]

  let find_by_name name = List.find_opt (fun c -> c.name = name) !all_
  let find_by_name_exn name = List.find (fun c -> c.name = name) !all_

  let register ~name ~compress ~decompress () : t  =
    match find_by_name name with
    | Some _ -> failwith (spf "codec '%s' already exists" name);
    | None ->
      let c = {name; compress; decompress} in
      all_ := c :: !all_;
      c

  let register' ~name ~compress ~decompress () : unit =
    ignore (register ~name ~compress ~decompress () : t)
end

module Decode = struct
  type 'a t = {
    input: Input.t;
    row: (Input.t -> 'a);
    mutable sync_marker: string; (* len=16 *)
    mutable first_block: bool;
    mutable is_done: bool;

    mutable codec: Codec.t; (* decompression *)

    mutable block_input: Input.t;
    (* an input specialized for the current block. Might be the
       same as {!input}. *)

    mutable block_remaining_count: int;
    (** How many items in the current block. *)
  }

  let read_header_ self : unit =
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
      | None -> Codec.null
      | Some name ->
        begin match Codec.find_by_name name with
          | Some c -> c
          | None -> failwith (spf "unknown codec %S" name)
        end
    in
    self.codec <- codec;
    (* read sync marker *)
    self.sync_marker <- Input.read_string_of_len self.input 16;
    ()

  let make input ~read:row : _ t =
    let self = {
      input; row; first_block=true; is_done=false;
      codec=Codec.null;
      block_input=input; sync_marker="";
      block_remaining_count=0;
    } in
    read_header_ self;
    self

  let[@inline] cur_block_remaining_count self = self.block_remaining_count

  (* fetch next block *)
  let next_block_ self : unit =

    (* read previous' block's sync marker, if we already parsed a block before *)
    if self.first_block then (
      self.first_block <- false;
    ) else (
      let syncm = Input.read_string_of_len self.input 16 in
      if syncm <> self.sync_marker then (
        failwith (spf "expected sync marker %S, got %S" self.sync_marker syncm);
      );
    );

    (* read count+size. This is where we might encounter End_of_file *)
    begin match Input.read_int self.input with
      | count ->
        self.block_remaining_count <- count;
        let byte_size = Input.read_int self.input in

        (* obtain an input for the block's data *)
        let block_input =
          if Codec.is_null_ self.codec then (
            self.input
          ) else (
            (* TODO: streaming decode? *)

            (* read [byte_size] into a string and decompress it to obtain the input *)
            let buf = Bytes.make byte_size '\x00' in
            Input.read_exact self.input buf 0 byte_size;

            let decompressed =
              self.codec.Codec.decompress (Bytes.unsafe_to_string buf) in

            Input.of_string decompressed
          )
        in
        self.block_input <- block_input;

      | exception End_of_file ->
        (* no other block *)
        self.is_done <- true;
    end;
    ()

  (* read next row *)
  let rec next_ self =
    if self.is_done then None
    else if self.block_remaining_count = 0 then (
      (* read next block and try again from it *)
      next_block_ self;
      (next_ [@tailcall]) self
    ) else (
      let r = self.row self.block_input in
      self.block_remaining_count <- self.block_remaining_count - 1;
      Some r
    )

  let[@inline] next self =
    if self.is_done then None else next_ self

  let to_array_list self : _ array list =
    let rec loop acc =
      if self.is_done then List.rev acc
      else if self.block_remaining_count = 0 then (
        (* read next block and try again from it *)
        next_block_ self;
        loop acc
      ) else (
        (* read many items at once *)
        let arr =
          Array.init self.block_remaining_count
            (fun _ -> self.row self.block_input)
        in
        self.block_remaining_count <- 0;
        loop (arr :: acc)
      )
    in loop []

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
  type 'a with_params =
    ?max_block_count:int ->
    ?buf_size:int ->
    ?pool:Iobuf.Pool.t ->
    ?codec:Codec.t ->
    'a

  type 'a t = {
    out: Output.t;
    write: Output.t -> 'a -> unit;
    schema: string;
    codec: Codec.t;

    pool: Iobuf.Pool.t;
    sync_marker: string; (* len=16 *)

    max_block_count: int;
    mutable closed: bool;
    mutable block_out: Output.t;
    mutable block_chain : Iobuf.Chain.t;
    mutable block_count: int;
  }

  exception Closed

  let write_header_ self =
    let module OH = Obj_container_header in
    let magic = "Obj\x01" in
    Output.write_string_of_len self.out 4 magic;
    let meta = OH.Str_map.(
        empty |> add "avro.schema" self.schema
        |> add "avro.codec" self.codec.Codec.name) in
    OH.write self.out meta;
    Output.write_string_of_len self.out 16 self.sync_marker;
    ()

  let make
      ?(max_block_count=50_000) ?(buf_size=16 * 1024) ?pool ?(codec=Codec.null)
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
    let block_out, block_chain = Output.of_iobuf_chain pool in

    let self = {
      out; write; schema;
      closed=false; pool; codec;
      max_block_count; sync_marker;
      block_out; block_chain; block_count=0;
    } in
    write_header_ self;
    self

  (* write a block to [out] *)
  let[@inline never] flush_block_ self : unit =

    if Codec.is_null_ self.codec then (
      (* codec=null *)
      let block_size = Iobuf.Chain.len self.block_chain in

      (* write block header *)
      Output.write_int self.out self.block_count;
      Output.write_int self.out block_size;

      (* write data *)
      Iobuf.Chain.iter self.block_chain
        ~f:(fun (buf:Iobuf.t) ->
            Output.write_slice self.out buf.b buf.i (Iobuf.len buf));
      Iobuf.Chain.dealloc self.block_chain; (* recycle buffers *)

    ) else (
      (* we need to compress the whole block chain *)
      let content = Iobuf.Chain.contents self.block_chain in
      Iobuf.Chain.dealloc self.block_chain;

      (* TODO: streaming compression, reusing buffers *)
      let compressed = self.codec.Codec.compress content in
      let block_size = String.length compressed in

      (* write block header *)
      Output.write_int self.out self.block_count;
      Output.write_int self.out block_size;

      (* write data *)
      Output.write_slice self.out (Bytes.unsafe_of_string compressed) 0 block_size;
    );

    self.block_count <- 0;

    (* create a new output for the next block *)
    begin
      let out, block_chain = Output.of_iobuf_chain self.pool in
      self.block_out <- out;
      self.block_chain <- block_chain;
    end;

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
    self.block_count <- 1 + self.block_count;
    self.write self.block_out x;
    post_push_ self

  let close self =
    if not self.closed then (
      if self.block_count > 0 then flush_block_ self;
      self.closed <- true;
    )

  let write_seq
      ?max_block_count ?buf_size ?pool ?codec
      ~schema ~write out seq : unit =
    let self =
      make ?max_block_count ?buf_size ?pool ?codec
        ~schema ~write out
    in
    Seq.iter (push self) seq;
    close self

  let write_seq_to_string
      ?max_block_count ?buf_size ?pool ?codec
      ~schema ~write seq : string =
    let buf = Buffer.create 1_024 in
    let out = Output.of_buffer buf in
    write_seq
      ?max_block_count ?buf_size ?pool ?codec
      ~schema ~write out seq;
    Buffer.contents buf
end

