(* generated by OCaml-avro schema compiler *)

module Str_map = Map.Make(String)

let schema = "{\"type\":\"map\",\"values\":\"bytes\"}"

type nonrec t = string Str_map.t

let read (input:Input.t) : t =
  (let len = Input.read_int input in
   let arr = Array.init len
     (fun _ -> let k = Input.read_string input in
      let v = Input.read_string input in k, v) in
   Array.fold_left (fun m (k,v) -> Str_map.add k v m) Str_map.empty arr)

let write (out:Output.t) (self:t) : unit =
  (Output.write_int out (Str_map.cardinal self);
   Str_map.iter
     (fun k self -> Output.write_string out k; Output.write_string out self) self)
