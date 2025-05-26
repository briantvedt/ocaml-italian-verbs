(* preparation *)

type number = Singular | Plural

type person = First | Second | Third

type category = Are | Ere | Ire

let describe = function
  | Are -> "first conjugation"
  | Ere -> "second conjugation"
  | Ire -> "third conjugation"

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

let present_indicative infinitive number person =
  let (stem, cat) = decompose infinitive in
    match (number, person) with
      | (Singular, First) -> stem ^ "o"
      | (Singular, Second) -> stem ^ "i"
      | (Singular, Third) -> stem ^ (match cat with Are -> "a" | _ -> "e")
      | (Plural, First) -> stem ^ "iamo"
      | (Plural, Second) -> stem ^ (match cat with Are -> "a" | Ere -> "e" | Ire -> "i") ^ "te"
      | (Plural, Third) -> stem ^ (match cat with Are -> "a" | _ -> "o") ^ "no"
