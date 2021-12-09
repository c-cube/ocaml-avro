
type t = {
  b: bytes;
  mutable off: int;
  mutable len: int;
}
type iobuf = t

let[@inline] len self = self.len
let[@inline] cap self = Bytes.length self.b
let[@inline] get self i =
  if i >= self.len then invalid_arg "Iobuf.get";
  Bytes.get self.b (self.off + i)

let[@inline] consume self n =
  assert (n <= self.len);
  self.off <- n + self.off;
  self.len <- self.len - n

module Pool = struct
  type t = {
    mutable bufs: iobuf list;
    buf_size: int;
  }

  let create ?(buf_size=16 * 1024) () : t =
    { buf_size; bufs=[] }

  let alloc (self:t) : iobuf =
    match self.bufs with
    | [] -> {b=Bytes.make self.buf_size '\x00'; off=0; len=0}
    | b :: tl ->
      self.bufs <- tl; b

  let recycle self b : unit =
    b.off <- 0; b.len <- 0;
    self.bufs <- b :: self.bufs
end
