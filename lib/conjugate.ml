type person = First | Second | Third

type number = Singular | Plural

(* constants *)

let a_grave = "\u{E0}"
let e_acute = "\u{E9}"
let i_grave = "\u{EC}"
let o_grave = "\u{F2}"

(* preparation *)

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

let present_participle infinitive =
  let (stem, cat) = decompose infinitive in
    stem ^ (match cat with Are -> "a" | _ -> "e") ^ "nte"

let past_participle infinitive =
  let (stem, cat) = decompose infinitive in
    stem ^ (match cat with Are -> "a" | Ere -> "u" | Ire -> "i") ^ "to"

let present_indicative infinitive person number =
  let (stem, cat) = decompose infinitive in
    match (person, number) with
    | (First, Singular) -> stem ^ "o"
    | (Second, Singular) -> stem ^ "i"
    | (Third, Singular) -> stem ^ (match cat with Are -> "a" | _ -> "e")
    | (First, Plural) -> stem ^ "iamo"
    | (Second, Plural) -> stem ^ (match cat with Are -> "a" | Ere -> "e" | Ire -> "i") ^ "te"
    | (Third, Plural) -> stem ^ (match cat with Are -> "a" | _ -> "o") ^ "no"

let imperfect infinitive person number =
  let (stem, cat) = decompose infinitive in
    let stemyv = stem ^ (match cat with Are -> "a" | Ere -> "e" | Ire -> "i") ^ "v" in
      match (person, number) with
      | (First, Singular) ->  stemyv ^ "o"
      | (Second, Singular) -> stemyv ^ "i"
      | (Third, Singular) -> stemyv ^ "a"
      | (First, Plural) -> stemyv ^ "amo"
      | (Second, Plural) -> stemyv ^ "ate"
      | (Third, Plural) -> stemyv ^ "ano"

let past_definite infinitive person number =
  let (stem, cat) = decompose infinitive in
    let stemy = stem ^ (match cat with Are -> "a" | Ere -> "e" | Ire -> "i") in
    let twist = (match cat with Are -> o_grave | Ere -> e_acute | Ire -> i_grave) in
      match (person, number) with
      | (First, Singular) -> stemy ^ "i"
      | (Second, Singular) -> stemy ^ "sti"
      | (Third, Singular) -> stem ^ twist
      | (First, Plural) -> stemy ^ "mmo"
      | (Second, Plural) -> stemy ^ "ste"
      | (Third, Plural) -> stemy ^ "rano"

let future infinitive person number =
  let (stem, cat) = decompose infinitive in
    let stemyr = stem ^ (match cat with Ire -> "i" | _ -> "e") ^ "r" in
      match (person, number) with
      | (First, Singular) -> stemyr ^ o_grave
      | (Second, Singular) -> stemyr ^ "ai"
      | (Third, Singular) -> stemyr ^ a_grave
      | (First, Plural) -> stemyr ^ "emo"
      | (Second, Plural) -> stemyr ^ "ete"
      | (Third, Plural) -> stemyr ^ "anno"

let conditional infinitive person number =
  let (stem, cat) = decompose infinitive in
    let stemyr = stem ^ (match cat with Ire -> "i" | _ -> "e") ^ "r" in
      match (person, number) with
      | (First, Singular) -> stemyr ^ "ei"
      | (Second, Singular) -> stemyr ^ "esti"
      | (Third, Singular) -> stemyr ^ "ebbe"
      | (First, Plural) -> stemyr ^ "emmo"
      | (Second, Plural) -> stemyr ^ "este"
      | (Third, Plural) -> stemyr ^ "ebbero"
