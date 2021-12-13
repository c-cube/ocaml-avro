
# Avro [![build](https://github.com/c-cube/ocaml-avro/actions/workflows/main.yml/badge.svg)](https://github.com/c-cube/ocaml-avro/actions/workflows/main.yml)

- `avro` is the runtime library, should be reasonably small (depends only on `camlzip`)
- `avro-compiler` can read a json schema and generate code from it

The online documentation can be found [here](https://c-cube.github.io/ocaml-avro/)

## Supported features

- [x] code generation from json schema (using `avro-compiler`)
  * **NOTE**: default values are not supported yet.
- [x] binary encoding
- [ ] schema evolution
  (in particular the schema is emitted as-is, nothing is stripped)
- [ ] sorting
- [ ] json encoding
- codecs:
  * [x] null
  * [x] deflate
  * [ ] snappy

## Examples

See some examples from the `tests/` directory:

- `tests/records.json` and `tests/records_test.ml` that use it.
  you can see what the schema compiler will produce:

  ```sh
  $ ./compiler.sh ./tests/records.json
  (* generated by avro-compiler *)

  open Avro

  module Str_map = Map.Make(String)

  let schema = "{\"type\":\"record\",\"name\":\"test\",\"fields\":[{\"type\":\"long\",\"name\":\"a\"},{
  \"type\":\"string\",\"name\":\"b\"}]}"

  type nonrec t = { a: int64;  b: string; }

  let read (input:Input.t) : t =
    (let a = Input.read_int64 input in
     let b = Input.read_string input in
     { a;b })

  let write (out:Output.t) (self:t) : unit =
    ((let self = self.a in Output.write_int64 out self);
      (let self = self.b in Output.write_string out self);
      )

  ```

- similarly for `tests/employee.json` and `tests/employee_test.ml` that use it

## License 

MIT license.
