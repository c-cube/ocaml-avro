
module Sc = Schema

let spf = Printf.sprintf
let aspf = Format.asprintf
let fpf = Format.fprintf
let pstr = Format.pp_print_string

(* taken from containers' CCFormat.string_lines *)
let ptext out s =
  fpf out "@[<v>";
  let i = ref 0 in
  let n = String.length s in
  while !i < n do
    let j =
      try String.index_from s !i '\n' with Not_found -> n in
    if !i>0 then fpf out "@,";
    pstr out (String.sub s !i (j - !i));
    i := j+1;
  done;
  fpf out "@]"

module Code = struct
  type t =
    | Top of string
    | Mod of string * t list

  let to_string (l:t list) : string =
    let rec pp out l =
      List.iteri (fun i s ->
          if i>0 then fpf out "@ @ ";
          match s with
          | Top s -> fpf out "@[%a@]" ptext s
          | Mod (name,l) ->
            Format.fprintf out "@[<v2>module %s = struct@ " name ;
            pp out l;
            Format.fprintf out "@ end@]@ ";
        )
        l;
    in
    Format.asprintf "@[<v>%a@]@." pp l

  let s str : t = Top str
  let sf fmt = Format.kasprintf s fmt

  let mod_ m l : t = Mod (m, l)
end

type state = {
  mutable code: Code.t list;
}

let push (self:state) c : unit = self.code <- c :: self.code

let ppdoc out = function
  | None -> ()
  | Some d -> fpf out "@ (** %s *)@ " d

(** Generate names for unions *)
let mk_gen_name () =
  let i = ref 0 in
  fun() -> let n= !i in incr i; spf "union_%d" n

type lprinter = Format.formatter -> unit
let lstr s out = pstr out s
let opt_or default = function None -> default | Some x -> x

type local_state = {
  gen_name: unit -> string;
  ty_name: string;
  names_for_self: string list;
  read_name: string;
  root: bool;
}

(** Traverse type, in the context of the toplevel declaration of [ty_name]. *)
let rec gen_rec ~root ~is_rec ~names_for_self ~ty_name ~read_name ~gen_name
    (self:state) ty : lprinter * lprinter =
  let recurse ?(names_for_self=names_for_self) ty =
    gen_rec self ty
      ~names_for_self ~is_rec ~root:false ~ty_name ~read_name ~gen_name
  in

  (* check if subtype named [name] is the current type *)
  let is_self name aliases =
    let matches s = List.mem s names_for_self in
    if
      not root &&
      (matches name ||
       (match aliases with Some l-> List.exists matches l | None -> false))
    then (
      is_rec := true;
      true
    ) else (
      false
    )
  in

  match ty with
  | Sc.Atomic a ->
    let tydecl, read = match a with
      | Sc.Null -> "unit", "()"
      | Sc.Bool -> "bool", "Input.read_bool input"
      | Sc.Int32 -> "int", "Input.read_int input"
      | Sc.Int64 -> "int64", "Input.read_int64 input"
      | Sc.Float32 -> "float", "Input.read_float32 input"
      | Sc.Float64 -> "float", "Input.read_float64 input"
      | Sc.String | Sc.Bytes -> "string", "Input.read_string input"
    in
    lstr tydecl, lstr read

  | Sc.Record {name; aliases; _}
  | Sc.Enum {name; aliases; _} when is_self name aliases ->
    (* recursive case *)
    lstr ty_name, (fun out -> fpf out "%s input" read_name)

  | Sc.Named name2 when is_self name2 None ->
    (* recursive case *)
    lstr ty_name, (fun out -> fpf out "%s input" read_name)

  | Sc.Named name2 ->
    lstr name2, (fun out -> fpf out "read_%s input" name2)

  | Sc.Array ty ->
    let ty_decl, ty_read = recurse ty in
    let ty out =
      fpf out "@[%t@ array@]" ty_decl
    and read out =
      fpf out
        "(@[<v>let len = Input.read_int input in@ \
         @[<2>Array.init len@ \
         (@[fun _ ->@ %t@])@]@])" ty_read
    in
    ty, read

  | Sc.Str_map ty ->
    let ty_decl, ty_read = recurse ty in
    let ty out = fpf out "@[%t@ Str_map.t@]" ty_decl in
    let read out =
      fpf out
        "(@[<v>\
         let len = Input.read_int input in@ \
         @[<2>let arr =@ Array.init len@ \
         (@[fun _ ->@ \
         let k = Input.read_string input in@ \
         let v = %t in@ \
         k, v@]) in@]@ \
         Array.fold_left (fun m (k,v) -> Str_map.add k v m) Str_map.empty arr@])"
        ty_read;
    in
    ty, read

  | Sc.Record {fields; name; aliases; doc; namespace=_ } ->
    if root then (
      let names_for_self = name :: opt_or [] aliases in
      let enc_fields =
        List.mapi
          (fun i (field:Sc.record_field) ->
             let tydecl, read = recurse ~names_for_self field.ty in

             let ty out =
               if i>0 then fpf out "@ ";
               fpf out "@[%s: %t@]; " field.name tydecl;
               ppdoc out field.doc
             and read out =
               fpf out "@[<2>let %s =@ %t in@]@ " field.name read
             in
             ty, read)
          fields
      in

      let tydecl out =
        fpf out "{@ ";
        List.iter (fun (ty,_) -> ty out) enc_fields;
        fpf out "@;<0 -2>}";
        ppdoc out doc;
      and read out =
        fpf out "(@[<v>";
        List.iter (fun (_,r) -> r out) enc_fields;
        fpf out "{ %s }@])" @@ (String.concat ";" @@ List.map (fun f->f.Sc.name) fields);
      in
      tydecl, read

    ) else (
      (* generate a type decl and refer to it *)
      let read_name' = spf "read_%s" name in
      gen_top self ~gen_name ~ty_name:name ~read_name:read_name' ty;

      lstr name, (fun out -> fpf out "%s input" read_name')
    )

  | Sc.Union (([Sc.Atomic Sc.Null; a] | [a; Sc.Atomic Sc.Null]) as l) ->
    (* basically an option *)
    let nullfirst = match l with | Sc.Atomic Sc.Null :: _ -> true | _ -> false in

    let adecl, aread = recurse a in
    let ty out =
      fpf out "@[%t@ option@]" adecl
    and read out =
      fpf out "(@[let idx = Input.read_int input in@ \
               if idx=%d then None@ else Some (@[%t@])@])"
        (if nullfirst then 0 else 1) aread
    in
    ty, read

  | Sc.Union l ->
    if root then (

      let l = List.map (fun ty -> ty, recurse ty) l in

      let ty out =
        fpf out "@ @[<v>@ ";
        List.iteri
          (fun i (ty, (decl,_)) ->
             match ty with
             | Sc.Fixed {name; _} | Sc.Record {name; _} | Sc.Named name
             | Sc.Enum {name; _} ->
               fpf out "@[| C_%s of@ %t @]@ " name decl
             | Sc.Union _ -> failwith "nested union";
             | Sc.Atomic _ | Sc.Array _ | Sc.Str_map _ ->
               fpf out "@[| C_%d of %t @]@" i decl
          )
          l;
        fpf out "@]";
      and read out =
        fpf out "(@[<v>let idx = Input.read_int input in@ \
                 match idx with@ ";
        List.iteri
          (fun i (ty,(_,read)) ->
             fpf out "| @[%d ->@ " i;
             match ty with
             | Sc.Fixed {name; _} | Sc.Record {name; _} | Sc.Named name
             | Sc.Enum {name; _} ->
               fpf out "C_%s@ (%t)@]@ " name read
             | Sc.Union _ -> failwith "nested union";
             | Sc.Atomic _ | Sc.Array _ | Sc.Str_map _ ->
               fpf out "C_%d %t@]@" i read
          )
          l;
        fpf out "| _ -> failwith \"bad index\"";
        fpf out "@])";
      in
      ty, read

    ) else (
      let name' = gen_name () in
      let read_name' = spf "read_%s" name' in
      gen_top self ~ty_name:name' ~read_name:read_name' ~gen_name ty;

      let ty out = pstr out name'
      and read out =
        fpf out "%s input" read_name'
      in

      ty, read
    )

  | Sc.Fixed { name; size; doc; namespace=_ } ->
    if root then (
      let ty out = pstr out name; ppdoc out doc;
      and read out =
        fpf out "Input.read_string_of_len %d" size
      in
      ty, read

    ) else (
      lstr name, (fun out -> fpf out "%s input" name)
    )

  | Sc.Enum { name; doc; symbols; aliases=_; namespace=_ } ->
    if root then (
      let ty out =
        fpf out "@ ";
        List.iter
          (fun s ->
             let cname = String.capitalize_ascii s in
             fpf out "@ | %s" cname)
          symbols;
        ppdoc out doc;
      and read out =
        fpf out "(@[<v>let idx = Input.read_int input in@ \
                 match idx with@ ";
        List.iteri
          (fun i s ->
             let cname = String.capitalize_ascii s in
             fpf out "| %d -> %s@ " i cname)
          symbols;
        fpf out "| _ -> failwith \"bad index\"";
        fpf out "@])"
      in
      ty, read

    ) else (
      let ty out = pstr out name
      and read out = fpf out "%s input" name in
      ty, read
    )

and gen_top (self:state) ~gen_name ~read_name ~ty_name (ty:Schema.t) : unit =
  let gen_name = mk_gen_name() in
  let is_rec = ref false in

  let decl, read =
    gen_rec self ~names_for_self:[]
      ~is_rec ~gen_name ~read_name ~ty_name ~root:true ty in
  push self (Code.sf "@[<hv2>type %s%s = %t@]"
               (if !is_rec then "" else "nonrec ") ty_name decl);
  push self (Code.sf "@[<hv2>let %s%s (input:Input.t) : %s =@ %t@]"
               (if !is_rec then "rec " else "")
               read_name ty_name read);
  ()

let gen ?(internal=false) (sc:Schema.t) : Code.t list =
  let self = { code=[]; } in
  push self (Code.sf "(* generated by avro-compiler *)@.");
  if not internal then push self (Code.sf "open Avro@.");
  push self (Code.sf "module Str_map = Map.Make(String)@.");
  push self (Code.sf "let schema = %S@." @@ Schema.to_string sc);

  let gen_name = mk_gen_name() in
  gen_top self ~gen_name ~ty_name:"t" ~read_name:"read" sc;

  List.rev self.code
