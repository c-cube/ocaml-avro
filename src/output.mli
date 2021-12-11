
(** A sink for bytes.

    Serializing objects always ends up writing bytes to some output.
*)

type t

val of_buffer : Buffer.t -> t

val of_chan : out_channel -> t

val with_file :
  ?flags:open_flag list ->
  ?mode:int ->
  string -> (t -> 'a) -> 'a

val of_iobuf_chain : Iobuf.Pool.t -> t * Iobuf.Chain.t
(** [of_iobuf_chain pool] is an output that writes to buffers allocated
    from [pool]. It also returns a chain of buffers into which the content
    is written *)

val write_byte : t -> char -> unit
(** Write a single byte. *)

val write_slice : t -> bytes -> int -> int -> unit
(** Write the given slice. *)

val write_bool : t -> bool -> unit
val write_int : t -> int -> unit
val write_int64 : t -> int64 -> unit
val write_float32 : t -> float -> unit
val write_float64 : t -> float -> unit
val write_string : t -> string -> unit
val write_string_of_len : t -> int -> string -> unit

val flush : t -> unit
(** Hint to flush underlying buffer/channel/socket. *)
