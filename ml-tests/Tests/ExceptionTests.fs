module ml_tests.Tests.ExceptionTests

open NUnit.Framework
open mini_ml_tests.Parser

[<TestFixture>]
type ExceptionTests () =
    
    [<TestCase("try 3 with 5", "3")>]
    [<TestCase("try true with false", "true")>]
    [<TestCase("try 3 / 0 with 5", "5")>]
    [<TestCase("try 3 % 0 with 5", "5")>]
    [<TestCase("try head [] with 5", "5")>]
    [<TestCase("try head [300, 1] with 5", "300")>]
    [<TestCase("try head [300, 1] with try 5 with 2", "300")>]
    [<TestCase("try 500 / 0 with try raise(test) with 2", "2")>]
    member this.TestReduction(actual: string, expected: string) =
        reduce_equals actual expected
    
    
    [<TestCase("try 3 with 5", "num")>]
    [<TestCase("try true with false", "bool")>]
    [<TestCase("raise(DivideByZero)", "a")>]
    [<TestCase("raise(custom)", "a")>]
    member this.TestTypes(actual: string, expected: string) =
        type_equals actual expected
