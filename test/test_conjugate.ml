
open Conjugate

let test_present_indicative () =
  let result = present_indicative "sentire" Second Plural in
  assert (result = "sentite")

let test_past_participle () = 
  let result = past_participle "credere" in
  assert (result = "creduto")

let () =
  test_present_indicative();
  test_past_participle()
