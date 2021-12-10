
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

let[@inline] write_char self c =
  if self.after_last >= cap self then invalid_arg "Iobuf.write_char";
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
