module mini_ml_tests
open FSharp.Text.Lexing
open NUnit.Framework
open Core.Type

[<TestFixture>]
type TypeTests () =
    member this.parse text =
        let lexbuf = LexBuffer<char>.FromString text
        let result = Parser.start Lexer.tokenstream lexbuf
        result
    
    [<TestCase("1")>]
    [<TestCase("1 + 3")>]
    [<TestCase("4 - 3")>]
    [<TestCase("7 * 3")>]
    [<TestCase("9 / 3")>]
    [<TestCase("10 % 3")>]
    [<TestCase("(x => x + 3) 3")>]
    [<TestCase("(x => y => x + y + 3) 3 6")>]
    member this.TestTNum(test: string) =
        let term = this.parse test
        let t = infer_type term

        match t with
        | TNum -> Assert.Pass()
        | _ -> Assert.Fail()
        
    // [<TestCase()>]
    // member this.TestTList() =
    //     let term = this.parse "[1; 2; 3]"
    //     let t = infer_type term

    //     match t with
    //     | TList(TNum) -> Assert.Pass()
    //     | _ -> Assert.Fail()
