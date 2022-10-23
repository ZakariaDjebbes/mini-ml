module mini_ml_tests.ParserTests
open NUnit.Framework
open Core.Type
open mini_ml_tests.Parser

[<TestFixture>]
type ParserTests () =
    
    [<TestCase("1")>]
    member this.TestTNum(test: string) =
        Assert.AreEqual(3, 3)
