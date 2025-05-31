(* preparation *)

type number = Singular | Plural

type person = First | Second | Third

type category = Are | Ere | Ire

let categorize infinitive =
  match String.sub infinitive (String.length infinitive - 3) 3 with
    | "are" -> Are
    | "ere" -> Ere
    | "ire" -> Ire
    | _ -> invalid_arg "invalid ending"

let decompose infinitive =
  if String.length infinitive < 4 then invalid_arg "too short"
  else (String.sub infinitive 0 (String.length infinitive - 3), categorize infinitive)

(* conjugation *)

let present_indicative infinitive person number =
  let (stem, cat) = decompose infinitive in
    match (person, number) with
      | (First, Singular) -> stem ^ "o"
      | (Second, Singular) -> stem ^ "i"
      | (Third, Singular) -> stem ^ (match cat with Are -> "a" | _ -> "e")
      | (First, Plural) -> stem ^ "iamo"
      | (Second, Plural) -> stem ^ (match cat with Are -> "a" | Ere -> "e" | Ire -> "i") ^ "te"
      | (Third, Plural) -> stem ^ (match cat with Are -> "a" | _ -> "o") ^ "no"
