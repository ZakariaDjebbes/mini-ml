module mini_ml_tests.Parser

open System.IO
open FSharp.Text.Lexing
open Core.Term
open Core.Type
open NUnit.Framework

let readFile path = 
    let file = File.OpenText path
    let text = file.ReadToEnd()
    file.Close()
    text

let parse text =
        let lexbuf = LexBuffer<char>.FromString ((readFile "file.zfs") + "\n" + text)
        let result = Parser.start Lexer.tokenstream lexbuf
        result
        
let reduce term =
        let alpha = alpha_convert term
        let res = evaluate alpha
        res
        
let infer term =
        let alpha = alpha_convert term
        let res = infer_type alpha
        res
        
let reduce_equals term expected =
        let expected = reduce (parse expected)
        let actual = reduce (parse term)
        Assert.AreEqual(expected, actual)
        
let type_equals term expected =
        let actual = infer (parse term)
        Assert.AreEqual(expected, string_of_type actual)