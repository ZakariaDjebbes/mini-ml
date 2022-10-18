module Core.Operators.InternalOperator

type InternalOperator =
    | Head
    | Tail
    | Cons
    | Not
    | Empty
    | Zero
    
// Gets the string of an internal operator to be used in the pretty printer.
let string_of_internal_operator op =
    match op with
    | Head -> "head"
    | Tail -> "tail"
    | Cons -> "cons"
    | Not -> "not"
    | Empty -> "empty"
    | Zero -> "zero"
    