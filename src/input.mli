
(** Input

    A source of bytes. *)

type t

val of_string : string -> t

val of_string_compressed_deflate : string -> t
(** Read from a string that is compressed using zlib.
    This will handle the decompression. *)

val of_chan : in_channel -> t
(** Input that reads from the given channel. *)

val with_file :
  ?flags:open_flag list ->
  string -> (t -> 'a) -> 'a
(** Opens file, call the function with an input feeding from the file,
    and makes sure to cleanup before returning the function's result. *)

(** Custom inputs *)
module type CUSTOM = sig
  val small_buf8 : bytes
  (** A small buffer, used in various places *)

  val read_byte : unit -> char
  (** Read a single byte
      @raise End_of_file if input is exhausted *)

  val read_exact : bytes -> int -> int -> unit
  (** [read_exact buf i len] reads [len] bytes into [buf], starting at offset [i]
      @raise End_of_file if input has less than [len] bytes *)
end

val of_custom : (module CUSTOM) -> t
(** User defined input *)

val read_byte : t -> char
(** [read_byte i] returns the next char, or raises
    @raise End_of_file if done *)

val read_exact : t -> bytes -> int -> int -> unit
(** [read_exact i buf off len] reads [len] bytes from [i]
    @raise End_of_file if less than [len] bytes are in the input. *)


val read_uint64 : t -> int64

val read_int64 : t -> int64

val read_int : t -> int

val read_bool : t -> bool

val read_string_of_len : t -> int -> string

val read_float32 : t -> float

val read_float64 : t -> float

val read_string : t -> string
(** Read length-prefixed string *)

val read_array : (t -> 'a) -> t -> 'a array

val read_map : (t -> 'a) -> t -> 'a Map.Make(String).t
