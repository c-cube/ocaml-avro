
type t = {
  b: bytes;
  mutable i: int;
  mutable after_last: int;
}
type iobuf = t

let[@inline] len self = self.after_last - self.i
let[@inline] cap self = Bytes.length self.b
let[@inline] get self i =
  let j = self.i + i in
  if j >= self.after_last then invalid_arg "Iobuf.get";
  Bytes.get self.b j

let[@inline] clear self = self.i <- 0; self.after_last <- 0
let[@inline] write_cap self = cap self - self.after_last

let[@inline] write_byte self c =
  if self.after_last >= cap self then invalid_arg "Iobuf.write_byte";
  Bytes.set self.b self.after_last c;
  self.after_last <- 1 + self.after_last

let[@inline] write_slice self b i len : unit =
  if len > write_cap self then invalid_arg "Iobuf.write_slice";
  Bytes.blit b i self.b self.after_last len;
  self.after_last <- len + self.after_last

let[@inline] consume self n =
  assert (n <= len self);
  self.i <- n + self.i

module Pool = struct
  type t = {
    mutable bufs: iobuf list;
    buf_size: int;
  }

  let create ?(buf_size=16 * 1024) () : t =
    { buf_size; bufs=[] }

  let alloc (self:t) : iobuf =
    match self.bufs with
    | [] -> {b=Bytes.make self.buf_size '\x00'; i=0; after_last=0}
    | b :: tl ->
      self.bufs <- tl; b

  let recycle self b : unit =
    clear b;
    self.bufs <- b :: self.bufs
end

module Chain = struct
  type t = {
    mutable first: iobuf;
    mutable last: iobuf;
    q: iobuf Queue.t;
    pool: Pool.t;
  }

  let create ~pool () : t =
    let buf = Pool.alloc pool in
    let q = Queue.create () in
    Queue.push buf q;
    { first=buf; last=buf; q; pool }

  let[@inline] first self = self.first
  let[@inline] last self = self.last

  let[@inline] iter self ~f = Queue.iter f self.q

  let len self =
    let n = ref 0 in
    iter self ~f:(fun buf -> n := !n + len buf);
    !n

  let dealloc self =
    iter self ~f:(fun buf -> clear buf; Pool.recycle self.pool buf);
    Queue.clear self.q

  let contents self =
    let total_len = len self in
    let result = Bytes.make total_len '\x00' in
    let i = ref 0 in
    iter self ~f:(fun buf ->
        let len_buf = buf.after_last - buf.i in
        Bytes.blit buf.b buf.i result !i len_buf;
        i := !i + len_buf);
    assert (!i = total_len);
    Bytes.unsafe_to_string result

  let[@inline never] add_buf_ self =
    let buf = Pool.alloc self.pool in
    Queue.push buf self.q;
    self.last <- buf

  let[@inline] write_byte self c =
    if write_cap self.last = 0 then add_buf_ self;
    write_byte self.last c

  (* write slice, possibly into separate buffers *)
  let[@unroll 1] rec write_slice_rec_ self b i len =
    if len>0 then (
      if write_cap self.last = 0 then add_buf_ self;
      let len' = min len (write_cap self.last) in
      assert (len' > 0);
      write_slice self.last b i len';
      if len>len' then write_slice_rec_ self b (i+len') (len-len')
    )

  let write_slice = write_slice_rec_
end
