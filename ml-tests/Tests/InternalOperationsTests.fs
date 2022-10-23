module ml_tests.Tests.InternalOperationsTests

open NUnit.Framework
open mini_ml_tests.Parser

[<TestFixture>]
type BinaryOperationsTests () =
    
    [<TestCase("head [3, 4]", "3")>]
    [<TestCase("tail [3, 4]", "[4]")>]
    [<TestCase("head [true, true, false]", "true")>]
    [<TestCase("tail [false, true, false]", "[true, false]")>]
    [<TestCase("not true", "false")>]
    [<TestCase("not false", "true")>]
    [<TestCase("not (not true)", "true")>]
    [<TestCase("not (not false)", "false")>]
    [<TestCase("not (not (not true))", "false")>]
    [<TestCase("not (not (not false))", "true")>]
    [<TestCase("empty []", "true")>]
    [<TestCase("empty [1]", "false")>]
    [<TestCase("empty [1, 2]", "false")>]
    [<TestCase("not (empty [1, 2])", "true")>]
    [<TestCase("not (head [1, 2] != 1)", "true")>]
    [<TestCase("head [1, 2] == 1", "true")>]
    [<TestCase("head [true, false] && false", "false")>]
    member this.TestInternalReduction(actual: string, expected: string) =
        reduce_equals actual expected
    
    
    [<TestCase("head [3, 4]", "num")>]
    [<TestCase("tail [3, 4]", "list num")>]
    [<TestCase("head [true, true, false]", "bool")>]
    [<TestCase("tail [false, true, false]", "list bool")>]
    [<TestCase("not true", "bool")>]
    [<TestCase("not false", "bool")>]
    [<TestCase("not (not true)", "bool")>]
    [<TestCase("not (not false)", "bool")>]
    [<TestCase("not (not (not true))", "bool")>]
    [<TestCase("not (not (not false))", "bool")>]
    [<TestCase("empty []", "bool")>]
    [<TestCase("empty [1]", "bool")>]
    [<TestCase("empty [1, 2]", "bool")>]
    [<TestCase("not (empty [1, 2])", "bool")>]
    [<TestCase("not (head [1, 2] != 1)", "bool")>]
    [<TestCase("head [1, 2] == 1", "bool")>]
    [<TestCase("head [true, false] && false", "bool")>]
    member this.TestTypes(actual: string, expected: string) =
        type_equals actual expected    
