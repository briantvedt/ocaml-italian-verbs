let run_one_verb infinitive =
  let f = Conjugate.present_indicative infinitive in
  let conjugations = [(f First Singular); (f Second Singular); (f Third Singular);
      (f First Plural); (f Second Plural); (f Third Plural)] in
  print_endline (infinitive ^ ": " ^ (String.concat ", " conjugations))
  
let () =
  ["parlare"; "credere"; "sentire"]
  |> List.iter run_one_verb
