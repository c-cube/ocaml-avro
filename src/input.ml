
module type S = sig
  val small_buf8 : bytes
  val read_byte : unit -> char
  val read_exact : bytes -> int -> int -> unit
end
module type CUSTOM = S

type t = (module S)

let of_custom x = x

let of_string str : t =
  let i = ref 0 in
  let module M = struct
    let small_buf8 = Bytes.make 8 '\x00'
    let read_byte () =
      if !i = String.length str then raise End_of_file;
      let c = str.[!i] in
      incr i;
      c

    let read_exact buf off len =
      if !i + len > String.length str then raise End_of_file;
      Bytes.blit_string str !i buf off len;
      i := !i + len
  end
  in (module M)

let of_string_compressed_deflate str : t =
  match Zip_helper.decompress ~header:false str with
  | Error (`Zlib e) ->
    failwith (Format.asprintf "Input: cannot read compressed string:@ %a"
                Zip_helper.pp_error e)
  | Ok str' ->
    of_string str'

let of_chan ic : t =
  let bufpool = Iobuf.Pool.create() in
  let iobuf = Iobuf.Pool.alloc bufpool in

  let[@inline never] refill (buf:Iobuf.t) =
    let n = input ic buf.b 0 (Iobuf.cap buf) in
    if n = 0 then raise End_of_file;
    buf.i <- 0;
    buf.after_last <- n;
  in

  let[@inline] read_byte () =
    if Iobuf.len iobuf = 0 then refill iobuf;
    let c = Iobuf.get iobuf 0 in
    Iobuf.consume iobuf 1;
    c
  in
  let[@unroll 1] rec read_exact buf off len =
    if len>0 then (
      if Iobuf.len iobuf = 0 then refill iobuf;
      let len' = min len (Iobuf.len iobuf) in
      Bytes.blit iobuf.b iobuf.i buf off len';
      Iobuf.consume iobuf len';
      read_exact buf (off+len') (len-len')
    )
  in

  let module M = struct
    let small_buf8 = Bytes.make 8 '\x00'
    let read_byte = read_byte
    let read_exact = read_exact
  end in (module M)

let with_file ?(flags=[Open_binary;  Open_rdonly]) file f =
  let ic = open_in_gen flags 0o000 file in
  let input = of_chan ic in
  try
    let x = f input in
    close_in ic;
    x
  with e ->
    close_in_noerr ic;
    raise e

let[@inline] read_byte (module B:S) = B.read_byte()
let[@inline] read_exact (module B:S) buf off len = B.read_exact buf off len

let read_string_of_len (self:t) len : string =
  let buf = Bytes.make len '\x00' in
  read_exact self buf 0 len;
  Bytes.unsafe_to_string buf

(* adapted from BARE *)

let read_uint64 (self:t) : int64 =
  let[@unroll 2] rec loop () =
    let c = Char.code (read_byte self) in
    if c land 0b1000_0000 <> 0 then (
      let rest = loop() in
      let c = Int64.of_int (c land 0b0111_1111) in
      Int64.(logor (shift_left rest 7) c)
    ) else (
      Int64.of_int c (* done *)
    )
  in
  loop()

let read_int64 (self:t) : int64 =
  let open Int64 in
  let i = read_uint64 self in
  let sign_bit = logand 0b1L i in (* true if negative *)
  let sign = equal sign_bit 0L in
  let res =
    if sign then (
      shift_right_logical i 1
    ) else (
      (* put sign back *)
      logor (shift_left 1L 63) (shift_right_logical (lognot i) 1)
    )
  in
  res

let[@inline] read_int self : int =
  let i = read_int64 self in
  Int64.to_int i

let[@inline] read_bool self : bool =
  let c = read_byte self in
  Char.code c = 1

let read_float64 (self:t) : float =
  let (module B) = self in
  read_exact self B.small_buf8 0 8;
  let i64 = Bytes.get_int64_le B.small_buf8 0 in
  Int64.float_of_bits i64

let read_float32 (self:t) : float =
  let (module B) = self in
  read_exact self B.small_buf8 0 4;
  let i32 = Bytes.get_int32_le B.small_buf8 0 in
  Int32.float_of_bits i32

let read_string (self:t) : string =
  let len = read_int self in
  read_string_of_len self len

let read_array readx self : _ array =
  let[@unroll 1] rec loop prev =
    let len = read_int self in
    if len=0 then prev
    else (
      let arr = Array.init len (fun _ -> readx self) in
      let arr2 = if prev=[||] then arr else Array.append prev arr in
      loop arr2
    )
  in
  loop [||]

let read_map readv self : _ Map.Make(String).t =
  let module M = Map.Make(String) in
  let[@unroll 1] rec loop m =
    let len = read_int self in
    if len=0 then !m
    else (
      for _i = 1 to len do
        let k = read_string self in
        let v = readv self in
        m := M.add k v !m
      done;
      loop m
    )
  in
  loop (ref M.empty)
