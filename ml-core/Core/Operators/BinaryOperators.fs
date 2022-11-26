module Core.Operators.BinaryOperators

/// Defines a set of operations that can be used on two numers.
/// Note that these don't create any type inconsistencies, since we only have one type of numbers (TNum)
/// If we were to add floats (TFloat) this would no longer work (we wouldn't be able to add Tnums and Tfloats)
/// We would need to either add new operations like OCaml's (+.)
/// Or like F# we could use a typeclass to define the operations
type NumOperator =
    | Plus // +
    | Minus // -
    | Times // *
    | Divide // /
    | Mod // %
/// Defines a set of operations that can be used on two booleans.
type BoolOperator =
    | And // &&
    | Or // ||
/// Defines a set of comparison operations that can be used on two Numbers.
type ComparisonOperator =    
    | Equals // ==
    | NotEquals // !=
    | LessThan // <
    | LessThanOrEqual // <=
    | GreaterThan // >
    | GreaterThanOrEqual // >=


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