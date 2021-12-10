

module Decode : sig
  type 'a t
  (** Decoder for values of type ['a] *)

  val make :
    Input.t ->
    (Input.t -> 'a) ->
    'a t

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

val register_decompression_codec :
  string -> decompress:(string -> string) -> unit
