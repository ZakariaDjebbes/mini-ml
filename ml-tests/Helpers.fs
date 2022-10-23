module mini_ml_tests.Parser

open FSharp.Text.Lexing
open Core.Term
open Core.Type
open NUnit.Framework

let parse text =
        let lexbuf = LexBuffer<char>.FromString text
        let result = Parser.start Lexer.tokenstream lexbuf
        result
        
let reduce term =
        let alpha = alpha_convert term
        let res, _ = reduce alpha
        res
        
let infer term =
        let alpha = alpha_convert term
        let res = infer_type alpha
        res
        
let reduce_equals term expected =
        let expected = parse expected
        let actual = reduce (parse term)
        Assert.AreEqual(expected, actual)
        
        
let rec type_of_string (str: string) =
        let words = str.Split [|' '|]
        let name = words[0]
        match name with
        | "num" -> TNum
        | "bool" -> TBool
        | "unit" -> TUnit
        | "var" -> TVar(words.[1])
        | "fun" -> TArr(type_of_string words[1], type_of_string words[2])
        | "list" -> TList(type_of_string words[1])
        | "ptr" -> TPointer(type_of_string words[1])
        | _ -> failwith "Unknown type"
        
let type_equals term expected =
        let actual = infer (parse term)
        Assert.AreEqual(type_of_string expected, actual)