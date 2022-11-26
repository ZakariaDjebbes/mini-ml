module Helpers.CommandLineParser

open CommandLine
/// A command line parser for F#, allows me to add flags to customize how the program runs !
/// Reference: https://github.com/commandlineparser/commandline
type options = {
    [<Option('f', "file", HelpText = "File path to the file to evaluate and infer. If no file is specified a mini-REPL will run instead.", Required = false)>] filePath : string;
    [<Option('l', "include-lib",HelpText = "If enabeled, a library file containing some predefined functions (map, range ...) will be loaded.", Default = false, Required = false)>] lib : bool
    [<Option('d', "debug", HelpText = "Displays alpha-conversion and detailed evaluations.", Default = false, Required = false)>] debug : bool
}
    
let inline (|Success|Help|Version|Fail|) (result : ParserResult<'a>) =
    match result with
    | :? Parsed<'a> as parsed -> Success(parsed.Value)
    | :? NotParsed<'a> as notParsed when notParsed.Errors.IsHelp() -> Help
    | :? NotParsed<'a> as notParsed when notParsed.Errors.IsVersion() -> Version
    | :? NotParsed<'a> as notParsed -> Fail(notParsed.Errors)
    | _ -> failwith "invalid parser result"
    
let parseArgs args =
    Parser.Default.ParseArguments<options>(args)
