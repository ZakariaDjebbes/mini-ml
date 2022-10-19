module Program

open System
open Core.Term
open Core.Type
open FSharp.Text.Lexing
open Exceptions.Errors
open Helpers.Logger
open Core.Operators.InternalOperator
open Core.Operators.BinaryOperators

let logger = Logger()

let parse text =
    let lexbuf = LexBuffer<char>.FromString text

    let result =
        Parser.start Lexer.tokenstream lexbuf

    result

// try
//         let mutable term: Term = BinaryOperation (Num 6, Num 3, GreaterThan)
//         logger.logWarning $"Term: %s{string_of_term term}"
//
//         term <- alpha_convert term
//         logger.logFatal $"Alpha converted: %s{string_of_term term}"
//
//         let infered = infer_type term
//         logger.logSuccess $"Type: %s{string_of_type infered}"
//
//         term <- evaluate term
//         logger.logInfo $"Reduced: %s{string_of_term term}"   
//
// with
// | e ->
//     logger.logError "\nError: "
//     match e with
//     | :? TimeoutException as ex -> logger.logError $"%s{ex.Message}"
//     | :? NotSupportedException as ex -> logger.logError $"%s{ex.Message}"
//     | :? MissingFieldException as ex -> logger.logError $"%s{ex.Message}"
//     | :? System.Data.InvalidExpressionException as ex -> logger.logError $"%s{ex.Message}"
//     | :? RecursiveTypeException -> logger.logError "Recursive type found in term"
//     | :? UnkownTypeException -> logger.logError "Couldn't find a target in the output of unification"
//     | _ -> logger.logError $"%A{e.Message}"

while true do
    try
            logger.Line <- false
            logger.logDefault"> "
            logger.Line <- true
            
            let mutable term = parse (Console.ReadLine())
            logger.logWarning $"Term: %s{string_of_term term}"

            term <- alpha_convert term
            logger.logFatal $"Alpha converted: %s{string_of_term term}"
            
            let infered = infer_type term
            logger.logSuccess $"Type: %s{string_of_type infered}"

            term <- evaluate term
            logger.logInfo $"Reduced: %s{string_of_term term}"    

    with
    | e ->
        logger.logError "\nError: "
        match e with
        | :? TimeoutException as ex -> logger.logError $"%s{ex.Message}"
        | :? NotSupportedException as ex -> logger.logError $"%s{ex.Message}"
        | :? MissingFieldException as ex -> logger.logError $"%s{ex.Message}"
        | :? System.Data.InvalidExpressionException as ex -> logger.logError $"%s{ex.Message}"
        | :? RecursiveTypeException -> logger.logError "Recursive type found in term"
        | :? UnkownTypeException -> logger.logError "Couldn't find a target in the output of unification"
        | _ -> logger.logError $"%A{e.Message}"