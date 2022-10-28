module ml_tests.Tests.BasicTermsTests

open NUnit.Framework
open mini_ml_tests.Parser

[<TestFixture>]
type BasicTermsTests () =
    
    [<TestCase("1", "num")>]
    [<TestCase("true", "bool")>]
    [<TestCase("false", "bool")>]
    [<TestCase("()", "unit")>]
    [<TestCase("'x'", "char")>]
    [<TestCase("fun x -> x", "(a -> a)")>]
    [<TestCase("(fun x -> x) 2", "num")>]
    member this.TestTypes(actual: string, expected: string) =
        type_equals actual expected    