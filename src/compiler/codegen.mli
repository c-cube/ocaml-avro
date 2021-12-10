
(** A representation of OCaml code *)
module Code : sig
  type t

  val to_string : t list -> string
end

val gen : ?internal:bool -> Schema.t -> Code.t list
(** Generate code for the given schema.
    @param internal if true, will not generate [open Avro]. Only useful if
      it's done in dune, or as part of avro itself.
*)
