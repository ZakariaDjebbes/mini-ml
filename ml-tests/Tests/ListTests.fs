module ml_tests.Tests.ListTests

open NUnit.Framework
open mini_ml_tests.Parser

[<TestFixture>]
type ListTests () =
    
    [<TestCase("3 :: 2 :: []", "[3, 2]")>]
    [<TestCase("[3, 2]", "(3 :: (2 :: []))")>]
    [<TestCase("[3, 2, ]", "(3 :: (2 :: []))")>]
    [<TestCase("head [3, 2, ]", "3")>]
    [<TestCase("head ['a', 'b']", "'a'")>]
    [<TestCase("tail ['a', 'b', 'c']", "['b', 'c']")>]
    [<TestCase("head (true :: false :: [])", "true")>]
    [<TestCase("tail (false :: true :: [])", "[true]")>]
    member this.TestInternalReduction(actual: string, expected: string) =
        reduce_equals actual expected
    
    
    [<TestCase("3 :: 2 :: []", "List<num>")>]
    [<TestCase("[3, 2]", "List<num>")>]
    [<TestCase("[3, 2, ]", "List<num>")>]
    [<TestCase("head [3, 2, ]", "num")>]
    [<TestCase("head (true :: false :: [])", "bool")>]
    [<TestCase("tail (false :: true :: [])", "List<bool>")>]
    [<TestCase("['t', 'a', 's']", "List<char>")>]
    [<TestCase("head ['t', 'a', 's']", "char")>]
    [<TestCase("tail ['t', 'a', 's']", "List<char>")>]
    [<TestCase("fun tail -> tail", "(a -> a)")>]
    member this.TestTypes(actual: string, expected: string) =
        type_equals actual expected    
