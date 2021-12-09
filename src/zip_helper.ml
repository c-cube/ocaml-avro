
type error = Truncated of string | Compression_error of string

let pp_error fmt error =
  match error with
  | Truncated content ->
      Format.fprintf fmt "Truncated content after %d bytes"
        (String.length content)
  | Compression_error msg -> Format.fprintf fmt "Compression error: %s" msg


let pp_zlib_error fmt wrapped =
  let `Zlib error = wrapped in
  pp_error fmt error


let error e = Error (`Zlib e)

let compress_zlib ?level ?(header= false) input output =
  let pos = ref 0 in
  let length = String.length input in
  let feed buf =
    let bytes = min (Bytes.length buf) (length - !pos) in
    Bytes.blit_string input !pos buf 0 bytes ;
    pos := !pos + bytes ;
    bytes
  in
  Zlib.compress ?level ~header feed output


let uncompress_zlib ?(header= false) input output =
  let pos = ref 0 in
  let length = String.length input in
  let feed buf =
    let bytes = min (Bytes.length buf) (length - !pos) in
    Bytes.blit_string input !pos buf 0 bytes ;
    pos := !pos + bytes ;
    bytes
  in
  Zlib.uncompress ~header feed output


let compress ?level ?header input =
  let compressed = Buffer.create 1_024 in
  let output buffer length =
    Buffer.add_subbytes compressed buffer 0 length
  in
  compress_zlib ?level ?header input output ;
  Buffer.contents compressed


let decompress ?header ?(max_size= Sys.max_string_length) input =
  let size = ref 0 in
  let uncompressed = Buffer.create 1_024 in
  let output buffer length =
    size := !size + length ;
    if !size < 0 then
      invalid_arg "Ezgzip: output larger than max string length" ;
    if !size > max_size then raise Exit
    else Buffer.add_subbytes uncompressed buffer 0 length
  in
  try
    uncompress_zlib ?header input output ;
    Ok (Buffer.contents uncompressed)
  with
  | Exit -> error (Truncated (Buffer.contents uncompressed))
  | Zlib.Error (func, msg) ->
      let message = Format.asprintf "in %s: %s" func msg in
      error (Compression_error message)
