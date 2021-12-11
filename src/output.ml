
module type S = sig
  val small_buf8 : bytes
  val write_byte : char -> unit
  val write_slice : bytes -> int -> int -> unit
  val flush : unit -> unit
end

type t = (module S)

let of_buffer (buf:Buffer.t) : t =
  let module M = struct
    let small_buf8 = Bytes.make 8 '\x00'
    let[@inline] write_byte c = Buffer.add_char buf c
    let[@inline] write_slice b i len = Buffer.add_subbytes buf b i len
    let flush _ = ()
  end in
  (module M)

let of_chan (oc: out_channel) : t =
  let pool = Iobuf.Pool.create () in
  let buf = Iobuf.Pool.alloc pool in

  (* write content of buffer to [oc] *)
  let[@inline never] flush_buf () =
    output oc buf.b buf.i (Iobuf.len buf);
    Iobuf.clear buf;
  in

  let module M = struct
    let small_buf8 = Bytes.make 8 '\x00'

    let[@inline] write_byte c =
      if Iobuf.write_cap buf = 0 then flush_buf();
      Iobuf.write_byte buf c

    let[@unroll 2] rec write_slice b i len =
      if len>0 then (
        let bcap = Iobuf.write_cap buf in
        if bcap = 0 then flush_buf();
        let len' = min len bcap in
        assert (len'>0);
        Iobuf.write_slice buf b i len';
        write_slice b (i+len') (len-len')
      )

    let flush () =
      flush_buf();
      flush oc
  end in
  (module M)

let with_file
    ?(flags=[Open_creat; Open_wronly; Open_binary; Open_trunc])
    ?(mode=0o644) filename f =
  let oc = open_out_gen flags mode filename in
  let out = of_chan oc in
  let (module B) = out in
  try
    let x = f out in
    B.flush (); flush oc; close_out oc;
    x
  with e ->
    B.flush (); flush oc; close_out_noerr oc;
    raise e

let of_iobuf_chain (pool:Iobuf.Pool.t) : t * Iobuf.Chain.t =
  let module IOC = Iobuf.Chain in
  let chain = IOC.create ~pool () in
  let module M = struct
    let small_buf8 = Bytes.make 8 '\x00'
    let[@inline] write_byte c = IOC.write_byte chain c
    let[@inline] write_slice b i len = IOC.write_slice chain b i len
    let flush _ = ()
  end
  in
  (module M), chain

let[@inline] write_byte (module O:S) c = O.write_byte c
let[@inline] flush (module O:S) = O.flush()
let[@inline] write_slice (module O:S) b i len = O.write_slice b i len

(* this code is adapted from BARE encoding *)

(* no need to check for overflow below *)
external unsafe_chr : int -> char = "%identity"

let write_uint64 (self:t) (i:int64) : unit =
  let module I = Int64 in
  let i = ref i in
  let continue = ref true in
  while !continue do
    let j = I.logand 0b0111_1111L !i in
    if !i = j then (
      continue := false;
      let j = I.to_int j in
      write_byte self (unsafe_chr j)
    ) else (
      (* set bit 8 to [1] *)
      let lsb = I.to_int (I.logor 0b1000_0000L j) in
      let lsb = (unsafe_chr lsb) in
      write_byte self lsb;
      i := I.shift_right_logical !i 7;
    )
  done

let[@inline] write_int64 (self:t) i =
  let open Int64 in
  let ui = logxor (shift_left i 1) (shift_right i 63) in
  write_uint64 self ui

let[@inline] write_int (self:t) i = write_int64 self (Int64.of_int i)

let[@inline] write_bool self x =
  write_byte self (if x then Char.chr 1 else Char.chr 0)

let write_float32 (self:t) (f:float) =
  let (module O) = self in
  let i = Int32.bits_of_float f in
  Bytes.set_int32_le O.small_buf8 0 i;
  write_slice self O.small_buf8 0 4

let write_float64 (self:t) (f:float) =
  let (module O) = self in
  let i = Int64.bits_of_float f in
  Bytes.set_int64_le O.small_buf8 0 i;
  write_slice self O.small_buf8 0 8

let data_of ~size self x =
  if size <> Bytes.length x then failwith "invalid length for Encode.data_of";
  Buffer.add_bytes self x

let write_string self s =
  write_int self (String.length s);
  write_slice self (Bytes.unsafe_of_string s) 0 (String.length s)

let write_string_of_len self len s =
  if String.length s <> len then failwith "write_string_of_len: wrong length";
  write_slice self (Bytes.unsafe_of_string s) 0 (String.length s)
