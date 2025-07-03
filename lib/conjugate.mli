
type person = First | Second | Third

type number = Singular | Plural

val present_participle : string -> string

val past_participle : string -> string

val present_indicative : string -> person -> number -> string

val imperfect: string -> person -> number -> string

val past_definite: string -> person -> number -> string

val future: string -> person -> number -> string

val conditional: string -> person -> number -> string
