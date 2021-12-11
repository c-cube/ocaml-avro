
(** The object container file storage format

    This format is used to store a collection of {b rows} which have
    the same type, using an efficient binary encoding and optional compression.
*)

(** Compression codecs for data blocks *)
module Codec : sig
  type t

  val null : t

  val deflate : t

  val find_by_name : string -> t option
  val find_by_name_exn : string -> t
  (** @raise Not_found if codec is not found *)

  val name : t -> string

  val register :
    name:string ->
    compress:(string -> string) ->
    decompress:(string -> string) ->
    unit -> t
  (** Register decompression codecs. Defaults are "null" and "deflate". *)

  val register' :
    name:string ->
    compress:(string -> string) ->
    decompress:(string -> string) ->
    unit -> unit
end

(** Decoding multiple rows from an input *)
module Decode : sig
  type 'a t
  (** Decoder for values of type ['a] *)

  val make :
    Input.t ->
    read:(Input.t -> 'a) ->
    'a t
  (** Decoder using the given input, and given row reading function. *)

  val cur_block_remaining_count : _ t -> int
  (** How many items remain in the current block
      (can be 0 at the beginning) *)

  val next : 'a t -> 'a option
  (** Read next row *)

  (** {2 High level API} *)

  val iter : 'a t -> f:('a -> unit) -> unit

  val fold : 'a t -> f:('b -> 'a -> 'b) -> init:'b -> 'b

  val to_seq : 'a t -> 'a Seq.t

  val to_list : 'a t -> 'a list

  val to_array : 'a t -> 'a array
end

(** Encoding multiple rows to an output *)
module Encode : sig
  (** Encoder *)
  type 'a t

  exception Closed

  type 'a with_params =
    ?max_block_count:int ->
    ?buf_size:int ->
    ?pool:Iobuf.Pool.t ->
    ?codec:Codec.t ->
    'a

  val make :
    (Output.t ->
     schema:string ->
     write:(Output.t -> 'a -> unit) -> 'a t)
    with_params
  (** make a new encoder.
      @param write the row type writer
      @param schema the json schema
      @param pool pool of buffers to use internally
      @param buf_size size of buffers if [pool] is not provided
      @param max_block_count forces a flush whenever there are that many elements
      in the current block. *)

  val push : 'a t -> 'a -> unit
  (** [push enc x] pushes a row into [enc].
      @raise Closed if the encoder is closed with {!close}. *)

  val cur_block_count : _ t -> int
  (** How many objects in the current block? *)

  val flush_block : _ t -> unit
    (** Flush current block to output *)

  val close : _ t -> unit
  (** [close enc] flushes the last block, if any, and renders the encoder
      unusable. *)

  (** {2 High level API} *)

  val write_seq :
    (schema:string ->
     write:(Output.t -> 'a -> unit) ->
     Output.t -> 'a Seq.t -> unit) with_params
  (** Encode the given sequence of rows into the output. *)

  val write_seq_to_string :
    (schema:string ->
     write:(Output.t -> 'a -> unit) ->
     'a Seq.t -> string) with_params
  (** Encode the given sequence of rows into a string *)
end
