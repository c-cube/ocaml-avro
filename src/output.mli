
(** A sink for bytes.

    Serializing objects always ends up writing bytes to some output.
*)

type t

val of_buffer : Buffer.t -> t
(** Output into the given buffer. Encoded data can be accessed
    using {!Buffer.contents}. *)

val of_chan : out_channel -> t
(** Write data to the given channel. *)

val with_file :
  ?flags:open_flag list ->
  ?mode:int ->
  string -> (t -> 'a) -> 'a
(** [with_file file f] opens [file], obtaining a channel [oc],
    calls [f out] with an output that writes to the channel [oc].
    When [f out] returns, this closes [oc] and returns the same as [f]. *)

val of_iobuf_chain : Iobuf.Pool.t -> t * Iobuf.Chain.t
(** [of_iobuf_chain pool] is an output that writes to buffers allocated
    from [pool]. It also returns a chain of buffers into which the content
    is written *)

(** Custom output *)
module type CUSTOM = sig
  val small_buf8 : bytes
  (** Small temporary buffer, used for lengths and the likes *)

  val write_byte : char -> unit
  (** Write a single byte *)

  val write_slice : bytes -> int -> int -> unit
  (** [write_slice b i len] writes the slice of length [len]
      of [b] starting at [i]. *)

  val flush : unit -> unit
  (** Non specified hint that the data may be flushed onto the disk
      or network. Doing nothing is acceptable. *)
end

val of_custom : (module CUSTOM) -> t
(** Make a custom output. *)

(** {2 Writing} *)

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
val write_array : (t -> 'a -> unit) -> t -> 'a array -> unit
val write_map : (t -> 'a -> unit) -> t -> 'a Map.Make(String).t -> unit

val flush : t -> unit
(** Hint to flush underlying buffer/channel/socket. *)
