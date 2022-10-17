module Core.Operators

type BinaryOperator =
    | Plus
    | Minus
    | Times
    | Divide
    | Mod
    | And
    | Or

type UnaryOperator =
    | Not

type InternalOperator =
    | Head
    | Tail
    | Cons

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
    
/// Gets the string of an unary operator to be used in the pretty printer.
let string_of_unary_operator op =
    match op with
    | Not -> "!"