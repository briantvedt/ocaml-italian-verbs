
open Conjugate

let () =
  let result = present_indicative "sentire" Second Plural in
  assert (result = "sentite")

