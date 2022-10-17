module Core.Operators

type BinaryOperator =
    | Plus
    | Minus
    | Times
    | Divide
    | Mod
    | And
    | Or

type InternalOperator =
    | Head
    | Tail
    | Cons
    | Not
    | Empty
    | Zero
/// Gets the string of an operator to be used in the pretty printer.
let string_of_binary_perator op =
    match op with
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Divide -> "/"
    | Mod -> "%"
    | And -> "&&"
    | Or -> "||"
    
// Gets the string of an internal operator to be used in the pretty printer.
let string_of_internal_operator op =
    match op with
    | Head -> "head"
    | Tail -> "tail"
    | Cons -> "cons"
    | Not -> "not"
    | Empty -> "empty"
    | Zero -> "zero"
    