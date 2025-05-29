
open Conjugate

let () =
  let result = present_indicative "sentire" Plural Second in
  assert (result = "sentite")

