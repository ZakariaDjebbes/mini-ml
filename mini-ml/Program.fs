module Program

open Core.Term
open Core.Type
open FSharp.Text.Lexing
open Exceptions.Errors
open Core.Operators

try
    let parse text =
        let lexbuf = LexBuffer<char>.FromString text
        let result = Parser.start Lexer.tokenstream lexbuf
        result
        
    // let mutable term : Term = InternalOperation(ConsList(ConsList(Num 3, ConsList (BinaryOperation(Num 6, Num 13, Plus), EmptyList)), ConsList(ConsList(Num 32, ConsList (Num 16, EmptyList)), EmptyList)), Tail)
    let mutable term : Term = Bool true
    // let mutable term : Term = InternalOperation(Var "x", Head)
    // let ex_k : Term = Abs ("x", Abs ("y", Var "x"))
    // let ex_s : Term = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))
    // let ex_omega : Term = App (Abs ("x", App (Var "x", Var "x")), Abs ("y", App (Var "y", Var "y")))
    // let my_term : Term = App(Abs("x", App(Var "x", Var "x")), Abs("x", App(Var "x", Var "x")))
    // let mutable term = App(App(Abs("x", Var "x"), Abs("x", Var "x")), Var "y");

    // let mutable term = parse "(x => x + 1) 1"
    printfn $"Term: %s{string_of_term term}"
    
    term <- alpha_convert term
    printfn $"Alpha converted Term: %s{string_of_term term}"
    
    let infered = infer_type term
    printfn $"Infered type: %s{string_of_type infered}"
    
    term <- evaluate term
    printfn $"Reduced: %s{string_of_term term}"
    
with e ->
    printfn "\n>>> Error: "
    match e with
    | :? System.TimeoutException as ex -> printfn $"%s{ex.Message}"
    | :? System.NotSupportedException as ex -> printfn $"%s{ex.Message}"
    | :? System.MissingFieldException as ex -> printfn $"%s{ex.Message}"
    | :? RecursiveTypeException -> printfn "Recursive type found in term"
    | :? TypeMismatchException as ex -> printfn $"%s{ex.Message}"
    | :? UnkownTypeException -> printfn "Couldn't find a target in the output of unification"
    | _ -> printfn $"%A{e.Message}"
