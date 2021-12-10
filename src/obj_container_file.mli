
(** The object container file storage format

    This format is used to store a collection of {b rows} which have
    the same type, using an efficient binary encoding. *)

(** Reading from an input *)
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

  val iter : 'a t -> f:('a -> unit) -> unit

  val fold : 'a t -> f:('b -> 'a -> 'b) -> init:'b -> 'b

  val to_seq : 'a t -> 'a Seq.t

  val to_list : 'a t -> 'a list

  val to_array : 'a t -> 'a array
end

module Encode : sig
  type 'a t

  exception Closed

  val make :
    ?max_block_count:int ->
    ?buf_size:int ->
    ?pool:Iobuf.Pool.t ->
    Output.t ->
    schema:string ->
    write:(Output.t -> 'a -> unit) -> 'a t
  (** make a new encoder.
      @param write the row type writer
      @param schema the json schema
      @param pool pool of buffers to use internally
      @param buf_size size of buffers if [pool] is not provided
      @param max_block_count forces a flush whenever there are that many elements
      in the current block. *)
  (* TODO: parameter for codec *)

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
end

val register_decompression_codec :
  string -> decompress:(string -> string) -> unit
(** Register decompression codecs. Defaults are "null" and "deflate". *)
