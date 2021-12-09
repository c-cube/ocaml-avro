
(** Input

    A source of bytes. *)

type t

val of_string : string -> t

val of_string_compressed_deflate : string -> t
(** Read from a string that is compressed using zlib.
    This will handle the decompression. *)

val of_chan : in_channel -> t

val read_byte : t -> char
(** [read_byte i] returns the next char, or raises
    @raise End_of_file if done *)

val read_exact : t -> bytes -> int -> int -> unit
(** [read_exact i buf off len] reads [len] bytes from [i]
    @raise End_of_file if less than [len] bytes are in the input. *)


val read_uint64 : t -> int64

val read_int64 : t -> int64

val read_int : t -> int

val read_string_of_len : t -> int -> string

val read_float32 : t -> float

val read_float64 : t -> float
