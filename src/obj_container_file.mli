

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
end

val register_decompression_codec :
  string -> decompress:(string -> string) -> unit
