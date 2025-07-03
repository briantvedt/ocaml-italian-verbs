open Conjugate

let all_forms f =
  let conjugations = [(f First Singular); (f Second Singular); (f Third Singular);
      (f First Plural); (f Second Plural); (f Third Plural)] in
    (String.concat ", " conjugations)

let run_one_verb infinitive =
  print_endline (infinitive ^ "/non_finite: " ^ (gerund infinitive) ^ ", "
        ^ (present_participle infinitive) ^ ", "
        ^ (past_participle infinitive));
  print_endline (infinitive ^ "/present: " ^ (all_forms (present_indicative infinitive)));
  print_endline (infinitive ^ "/imperfect: " ^ (all_forms (imperfect infinitive)));
  print_endline (infinitive ^ "/past_definite: " ^ (all_forms (past_definite infinitive)));
  print_endline (infinitive ^ "/future: " ^ (all_forms (future infinitive)));
  print_endline (infinitive ^ "/conditional: " ^ (all_forms (conditional infinitive)))

let () =
  ["parlare"; "credere"; "sentire"]
  |> List.iter run_one_verb
