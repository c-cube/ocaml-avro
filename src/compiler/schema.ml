
module J = Yojson.Safe

type atomic_type =
  | Null
  | Bool
  | Int32
  | Int64
  | Float32
  | Float64
  | Bytes
  | String

type top_type =
  | Atomic of atomic_type
  | Named of string
  | Array of top_type
  | Str_map of top_type
  | Union of top_type list
  | Fixed of {
      name: string;
      namespace: string option; (* TODO *)
      doc: string option;
      size: int;
    }
  | Enum of {
      name: string;
      namespace: string option; (* TODO *)
      symbols: string list;
      doc: string option;
      aliases: string list option;
    }
  | Record of {
      name: string;
      namespace: string option; (* TODO *)
      fields: record_field list;
      doc: string option;
      aliases: string list option;
    }

and record_field = {
  name: string;
  doc: string option;
  ty: top_type;
  (* TODO:
     default: value option;
     *)
}

(** A toplevel schema. *)
type t = top_type

let json_of_atomic (self:atomic_type) : J.t =
  match self with
  | Null -> `String "null"
  | Bool -> `String "boolean"
  | Int32 -> `String "int"
  | Int64 -> `String "long"
  | Float32 -> `String "float"
  | Float64 -> `String "double"
  | Bytes -> `String "bytes"
  | String -> `String "string"

let rec to_json (self:t) : J.t =
  let module JU = J.Util in
  match self with
  | Atomic i -> json_of_atomic i
  | Named s -> `String s
  | Union l -> `List (List.map to_json l)
  | Array ty ->
    `Assoc ["type", `String "array"; "items", to_json ty]
  | Str_map ty ->
    `Assoc ["type", `String "map"; "values", to_json ty]
  | Fixed { name; doc; namespace; size; } ->
    `Assoc (Util.list_keep_some [
        Some ("type", `String "fixed");
        Some ("name", `String name);
        Some ("size", `Int size);
        Util.map_opt doc ~f:(fun s -> "type", `String s);
        Util.map_opt namespace ~f:(fun s -> "namespace", `String s);
      ])
  | Enum { name; doc; symbols; namespace; aliases; } ->
    `Assoc (Util.list_keep_some [
        Some ("type", `String "enum");
        Some ("name", `String name);
        Some ("symbols", `List (List.map (fun s -> `String s) symbols));
        Util.map_opt doc ~f:(fun s -> "type", `String s);
        Util.map_opt namespace ~f:(fun s -> "namespace", `String s);
        Util.map_opt aliases ~f:(fun l -> "aliases", `List (List.map (fun s->`String s) l));
      ])
  | Record { name; doc; fields; namespace; aliases; } ->
    `Assoc (Util.list_keep_some [
        Some ("type", `String "record");
        Some ("name", `String name);
        Some ("fields", `List (List.map json_of_field fields));
        Util.map_opt doc ~f:(fun s -> "type", `String s);
        Util.map_opt aliases ~f:(fun l -> "aliases", `List (List.map (fun s->`String s) l));
        Util.map_opt namespace ~f:(fun s -> "namespace", `String s);
      ])

and json_of_field (f:record_field) : J.t =
  let {doc; name; ty; } = f in
  `Assoc (Util.list_keep_some [
        Some ("type", to_json ty);
        Some ("name", `String name);
        Util.map_opt doc ~f:(fun s -> "type", `String s);
      ])

let of_json (j:J.t) : (t, string) result =
  let exception E of string in
  let module JU = J.Util in
  let spf = Printf.sprintf in

  let assoc k l =
    try List.assoc k l
    with Not_found -> raise (E (spf "cannot find key %S" k))
  in

  let rec loop j : t =
    match j with
    | `String "null" -> Atomic Null
    | `String "boolean" -> Atomic Bool
    | `String "int" -> Atomic Int32
    | `String "long" -> Atomic Int64
    | `String "float" -> Atomic Float32
    | `String "double" -> Atomic Float64
    | `String "string" -> Atomic String
    | `String "bytes" -> Atomic Bytes
    | `String n -> Named n
    | `List ([] | [_]) -> failwith "union needs >= 2 elements"
    | `List l -> Union (List.map loop l)
    | `Assoc l ->
      begin try
          begin match assoc "type" l |> JU.to_string with
            | "array" ->
              let items = assoc "items" l |> loop in
              Array items

            | "map" ->
              let values = assoc "values" l |> loop in
              Str_map values

            | "fixed" ->
              let name = assoc "name" l |> JU.to_string in
              let namespace = None in (* TODO *)
              let doc = List.assoc_opt "doc" l |> Util.map_opt ~f:JU.to_string in (* TODO *)
              let size = assoc "size" l |> JU.to_int in
              Fixed {name; doc; namespace; size}

            | "record" ->
              let name = assoc "name" l |> JU.to_string in
              let namespace = None in (* TODO *)
              let aliases = None in (* TODO *)
              let doc = List.assoc_opt "doc" l |> Util.map_opt ~f:JU.to_string in (* TODO *)
              let fields = assoc "fields" l |> JU.to_list |> List.map loop_field in
              Record {name; namespace; doc; fields; aliases }

            | "enum" ->
              let name = assoc "name" l |> JU.to_string in
              let namespace = None in (* TODO *)
              let aliases = None in (* TODO *)
              let doc = List.assoc_opt "doc" l |> Util.map_opt ~f:JU.to_string in (* TODO *)
              let symbols = assoc "symbols" l |> JU.to_list |> List.map JU.to_string in
              Enum {name; namespace; doc; aliases; symbols }

            | s -> raise (E (spf "unknown schema type %S" s))
          end
        with e ->
          raise (E (spf "error %s\nwhile decoding %s" (Printexc.to_string e) (J.to_string j)))
      end
    | _ -> raise (E (spf "unknown json schema %s" (J.to_string j)))

  and loop_field j : record_field =
    let l = JU.to_assoc j in
    let name = assoc "name" l |> JU.to_string in
    let ty = assoc "type" l |> loop in
    let doc = List.assoc_opt "doc" l |> Util.map_opt ~f:JU.to_string in (* TODO *)
    { name; ty; doc; }
  in
  try Ok (loop j)
  with E e -> Error e

let pp out (self:t) = J.pretty_print out (to_json self)

let to_string (self:t) : string = J.to_string (to_json self)

let parse_string (str:string) : (t, string) result =
  match J.from_string str with
  | j -> of_json j
  | exception _ -> Error "cannot parse json"

let parse_file (file:string) : (t, string) result =
  match J.from_file file with
  | j -> of_json j
  | exception _ -> Error (Printf.sprintf "cannot parse json from file %S" file)

