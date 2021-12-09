
(** Encoding/Decoding for Apache Avro

    See https://avro.apache.org/docs/1.7.6/spec.html *)

module Input = Input

type 'a deser = Input.t -> 'a
(** A deserializer for values of type ['a].
    This is used by code-generation. *)

(**/**)
module Zip_helper = Zip_helper
(**/**)

