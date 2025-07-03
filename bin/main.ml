open Conjugate

let all_forms f =
  let conjugations = [(f First Singular); (f Second Singular); (f Third Singular);
      (f First Plural); (f Second Plural); (f Third Plural)] in
    (String.concat ", " conjugations)

let run_one_verb infinitive =
  print_endline ("[" ^ infinitive ^ "] " ^ (gerund infinitive) ^ " - "
        ^ (present_participle infinitive) ^ " - "
        ^ (past_participle infinitive));
  print_endline ("PRESENTE: " ^ (all_forms (present_indicative infinitive)));
  print_endline ("IMPERFETTO: " ^ (all_forms (imperfect infinitive)));
  print_endline ("PASSATO REMOTO: " ^ (all_forms (past_definite infinitive)));
  print_endline ("FUTURO: " ^ (all_forms (future infinitive)));
  print_endline ("CONDIZIONALE: " ^ (all_forms (conditional infinitive)));
  print_endline ""

let () =
  ["parlare"; "credere"; "sentire"]
  |> List.iter run_one_verb
