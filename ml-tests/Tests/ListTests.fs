module ml_tests.Tests.ListTests

open NUnit.Framework
open mini_ml_tests.Parser

[<TestFixture>]
type ListTests () =
    
    [<TestCase("3 :: 2 :: []", "[3, 2]")>]
    [<TestCase("[3, 2]", "(3 :: (2 :: []))")>]
    [<TestCase("[3, 2, ]", "(3 :: (2 :: []))")>]
    [<TestCase("head [3, 2, ]", "3")>]
    [<TestCase("head (true :: false :: [])", "true")>]
    [<TestCase("tail (false :: true :: [])", "[true]")>]
    member this.TestInternalReduction(actual: string, expected: string) =
        reduce_equals actual expected
    
    
    [<TestCase("3 :: 2 :: []", "list num")>]
    [<TestCase("[3, 2]", "list num")>]
    [<TestCase("[3, 2, ]", "list num")>]
    [<TestCase("head [3, 2, ]", "num")>]
    [<TestCase("head (true :: false :: [])", "bool")>]
    [<TestCase("tail (false :: true :: [])", "list bool")>]
    [<TestCase("fun tail -> tail", "arr a a")>]
    member this.TestTypes(actual: string, expected: string) =
        type_equals actual expected    
