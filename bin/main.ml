let run_one_verb infinitive =
  let f = present_indicative infinitive in
  let conjugations = [(f Singular First); (f Singular Second); (f Singular Third);
      (f Plural First); (f Plural Second); (f Plural Third)] in
  print_endline (infinitive ^ ": " ^ (String.concat ", " conjugations))
  
let () =
  ["parlare"; "credere"; "sentire"]
  |> List.iter run_one_verb
