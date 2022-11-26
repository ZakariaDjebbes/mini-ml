module CoreProgram

open System
open Core.Term
open Core.Type
open FSharp.Text.Lexing
open Exceptions.Errors
open Helpers.Logger
open System.IO
open Helpers.CommandLineParser

let logger = Logger()

let parse text =
    let lexbuf = LexBuffer<char>.FromString text

    let result =
        Parser.start Lexer.tokenstream lexbuf

    result

let readFile path =
    let file = File.OpenText path
    let text = file.ReadToEnd()
    file.Close()
    text

let infer_and_eval text lib =
    let mutable text = text
    if lib then
        let my_lib = readFile "file.zfs"
        text <- my_lib + text
    
    let term = parse text
    let alpha_term = alpha_convert term
    let infered_type = infer_type alpha_term
    let evaluated_term = evaluate alpha_term
    
    (term, alpha_term, infered_type, evaluated_term)

let run_file opts =
    let file = readFile opts.filePath
    
    let term, alpha_term, infered_type, evaluated_term = infer_and_eval file opts.lib
            
    if infered_type <> TUnit then
        logger.logError $"Your program was expected to return a unit type but returned %s{string_of_type infered_type} instead."
    else
        if opts.debug then
            logger.logSuccess $"Term: %s{string_of_term_debug term}"
            logger.logWarning $"Alpha converted: %s{string_of_term_debug alpha_term}"
            logger.logFatal $"Type: %s{string_of_type_debug infered_type}"
            logger.logInfo $"Reduced: %s{string_of_term_debug evaluated_term}"
        else
            logger.logSuccess $"Type: %s{string_of_type_debug infered_type}"
            logger.logFatal $"Reduced: %s{string_of_term_debug evaluated_term}"
    
let repl opts =
    logger.logWarning "Welcome to ZFS Repl : "

    while true do
        try
            logger.Line <- false
            logger.logDefault "> "
            logger.Line <- true

            let input = Console.ReadLine()

            let term, alpha_term, infered_type, evaluated_term = infer_and_eval input opts.lib
                    
            if opts.debug then
                logger.logSuccess $"Term: %s{string_of_term_debug term}"
                logger.logWarning $"Alpha converted: %s{string_of_term_debug alpha_term}"
                logger.logFatal $"Type: %s{string_of_type_debug infered_type}"
                logger.logInfo $"Reduced: %s{string_of_term_debug evaluated_term}"
            else
                logger.logSuccess $"Type: %s{string_of_type infered_type}"
                logger.logFatal $"Reduced: %s{string_of_term evaluated_term}"
        with
        | e ->
            match e with
            | :? TimeoutException as ex -> logger.logError $"%s{ex.Message}"
            | :? NotSupportedException as ex -> logger.logError $"%s{ex.Message}"
            | :? MissingFieldException as ex -> logger.logError $"%s{ex.Message}"
            | :? System.Data.InvalidExpressionException as ex -> logger.logError $"%s{ex.Message}"
            | :? RecursiveTypeException -> logger.logError "Recursive type found in term"
            | :? UnkownTypeException -> logger.logError "Couldn't find a target in the output of unification"
            | _ -> logger.logError $"Error : %A{e.Message}"

[<EntryPoint>]
let main args =
    let opts = parseArgs args

    match opts with
    | Success opts ->
        if String.IsNullOrEmpty opts.filePath then
            repl opts
        else
            run_file opts
        0
    | Fail errs ->
        logger.logError $"Invalid: %A{args}, Errors: %u{Seq.length errs}\n"
        0
    | Help
    | Version -> 0
