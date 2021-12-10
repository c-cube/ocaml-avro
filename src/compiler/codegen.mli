
(** A representation of OCaml code *)
module Code : sig
  type t

  val to_string : t list -> string
end

val gen : ?internal:bool -> Schema.t -> Code.t list
