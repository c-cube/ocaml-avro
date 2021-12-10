
type t = {
  b: bytes;
  mutable i: int;
  mutable after_last: int;
}
type iobuf = t

val len : t -> int
(** Length of existing content *)

val clear : t -> unit
(** Clear content, set [i=0], [after_last=0] *)

val cap : t -> int
(** Internal size of buffer *)

val get : t -> int -> char
(** Get content at index [i] (relative to internal offset {!t.i}) *)

val write_cap : t -> int
(** How much can we write *)

val write_char : t -> char -> unit
(** Write a single char. Assumes [write_cap buf > 0] *)

val write_slice : t -> bytes -> int -> int -> unit
(** [write_slice buf b i len] writes a slice of [b] to [buf].
    Assumes [len <= write_cap buf]. *)

val consume : t -> int -> unit

module Pool : sig
  type t

  val create : ?buf_size:int -> unit -> t

  val alloc : t -> iobuf

  val recycle : t -> iobuf -> unit
end
