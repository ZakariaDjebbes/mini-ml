module Core.Operators.BinaryOperators

type NumOperator =
    | Plus
    | Minus
    | Times
    | Divide
    | Mod
type BoolOperator =
    | And
    | Or
type ComparisonOperator =    
    | Equals
    | NotEquals
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual


/// Gets the string of a num operator to be used in the pretty printer.
let string_of_num_operator op =
    match op with
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Divide -> "/"
    | Mod -> "%"

/// Gets the string of a bool operator to be used in the pretty printer.
let string_of_bool_operator op =
    match op with
    | And -> "&&"
    | Or -> "||"

/// Gets the string of a comparison operator to be used in the pretty printer.
let string_of_comparison_operator op =
    match op with
    | Equals -> "=="
    | NotEquals -> "!="
    | LessThan -> "<"
    | LessThanOrEqual -> "<="
    | GreaterThan -> ">"
    | GreaterThanOrEqual -> ">="