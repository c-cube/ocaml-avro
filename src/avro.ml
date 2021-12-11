
(** Encoding/Decoding for Apache Avro

    See https://avro.apache.org/docs/1.7.6/spec.html *)

module Input = Input
module Output = Output
module Obj_container_file = Obj_container_file

(** Read list from file *)
let read_file_l (file:string) ~(read:Input.t->'a) : 'a list =
  let module OC = Obj_container_file in
  Input.with_file file @@ fun input ->
  let dec = OC.Decode.make input ~read in
  OC.Decode.to_list dec

(** Write seq to file *)
let write_file_seq (file:string) ~schema ~(write:Output.t -> 'a -> unit) (seq:'a Seq.t) : unit =
  let module OC = Obj_container_file in
  Output.with_file file @@ fun out ->
  OC.Encode.write_seq ~schema ~write out seq

(**/**)
module Zip_helper = Zip_helper
(**/**)

