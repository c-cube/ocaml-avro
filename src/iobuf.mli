
type t = {
  b: bytes;
  mutable off: int;
  mutable len: int;
}
type iobuf = t

val len : t -> int
val cap : t -> int
val get : t -> int -> char

val consume : t -> int -> unit

module Pool : sig
  type t

  val create : ?buf_size:int -> unit -> t

  val alloc : t -> iobuf

  val recycle : t -> iobuf -> unit
end
