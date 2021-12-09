
(* NOTE:

   this is imported from ezgzip (https://github.com/hcarty/ezgzip),
   which is also MIT licensed.

   This code belongs to hcarty and is extracted from Ezgzip.Z.
*)

(** {1 zlib compression} *)

(** Possible error cases *)
type error =
  | Truncated of string
      (** Extracted size is greater than the allowed maximum size *)
  | Compression_error of string  (** zlib error *)

val compress : ?level:int -> ?header:bool -> string -> string
(** [compress ?level ?header input] will return a zlib-compressed
    representation of [input]. *)

val decompress :
  ?header:bool -> ?max_size:int -> string
  -> (string, [> `Zlib of error]) result
(** [decompress ?header ?max_size input] will return a decompressed
    representation of [input].

    @return [Error `Zlib Compression_error message] if {!Zlib.Error} was
    raised while inflating [input].
    @return [Error `Zlib Truncated incomplete] if [input] inflates to more
    than [max_size] bytes.  [incomplete] contains less than [max_size] bytes
    of inflated content from [input]. *)

val pp_error : Format.formatter -> error -> unit

val pp_zlib_error : Format.formatter -> [`Zlib of error] -> unit
