
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

val write_byte : t -> char -> unit
(** Write a single char.
    @raise Invalid_argument if [write_cap buf = 0] *)

val write_slice : t -> bytes -> int -> int -> unit
(** [write_slice buf b i len] writes a slice of [b] to [buf].
    @raise Invalid_argument if [len > write_cap buf]. *)

val consume : t -> int -> unit

(** Pool of buffers, to reuse them once they're consumed *)
module Pool : sig
  type t

  val create : ?buf_size:int -> unit -> t

  val alloc : t -> iobuf

  val recycle : t -> iobuf -> unit
end

(** A chain of buffers stringed together *)
module Chain : sig
  type t

  val create : pool:Pool.t -> unit -> t

  val first : t -> iobuf
  val last : t -> iobuf

  val len : t -> int
  (** Sum of length of buffers *)

  val iter : t -> f:(iobuf -> unit) -> unit

  val dealloc : t -> unit
  (** Release all buffers. Do not use after calling that. *)

  val write_byte : t -> char -> unit
  (** Write a single char at the end. *)

  val write_slice : t -> bytes -> int -> int -> unit
  (** Write a slice at the end. *)

  val contents : t -> string
  (** Turn whole contents into a single string *)
end
