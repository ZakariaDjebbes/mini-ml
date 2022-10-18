module Core.Operators.BinaryOperator

type BinaryOperator =
    | Plus
    | Minus
    | Times
    | Divide
    | Mod
    | And
    | Or

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
    