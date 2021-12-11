
# Avro [![build](https://github.com/c-cube/ocaml-avro/actions/workflows/main.yml/badge.svg)](https://github.com/c-cube/ocaml-avro/actions/workflows/main.yml)

- `avro` is the runtime library, should be reasonably small (depends only on `camlzip`)
- `avro-compiler` can read a json schema and generate code from it

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


## Perf analysis

on branch `feat-tracy`, with `tracy-client` installed:

- run tracy in the background, click "connect"
- `make perfs; and ./employee_test.exe`

On my machine, a thinkpad (AMD Ryzen 7 PRO 3700U with 6020216 kB RAM, and a SSD),
I get `./employee_test.exe` to run in 4.7s to serialize 2M records using "deflate",
then read them back from disk. 2.05s are spent in writing, 2.64s in reading.
