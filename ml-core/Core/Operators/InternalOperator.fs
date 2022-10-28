module Core.Operators.InternalOperator

type InternalOperator =
    | Head
    | Tail
    | Not
    | Empty
    | NumToChar
    | CharToNum
    | Fst
    | Snd
    
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