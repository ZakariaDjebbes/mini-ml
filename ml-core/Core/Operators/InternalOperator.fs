module Core.Operators.InternalOperator

/// Represents a set of functions that are intrenal to the core of the language.
/// These are not defnied by the grammar or by the user.
type InternalOperator =
    | Head // head of a list
    | Tail // tail of a list
    | Not // logical not
    | Empty // check if a list is empty
    | NumToChar // convert a number to a character
    | CharToNum // convert a character to a number
    | Fst // get the first element of a pair
    | Snd // get the second element of a pair
    
// Gets the string of an internal operator to be used in the pretty printer.
let string_of_internal_operator op =
    match op with
    | Head -> "head"
    | Tail -> "tail"
    | Not -> "not"
    | Empty -> "empty"
    | NumToChar -> "num_to_char"
    | CharToNum -> "char_to_num"
    | Fst -> "fst"
    | Snd -> "snd"
    
/// Gets the internal operator from a string, used to parse the language. Since as i said these are not defnied by a grammar, but the grammar should be able to parse them.    
let internal_operator_of_string op =
    match op with
    | "head" -> Some(Head)
    | "tail" -> Some(Tail)
    | "not" -> Some(Not)
    | "empty" -> Some(Empty)
    | "num_to_char" -> Some(NumToChar)
    | "char_to_num" -> Some(CharToNum)
    | "fst" -> Some(Fst)
    | "snd" -> Some(Snd)
    | _ -> None