module ml_tests.Tests.ParserTests
open NUnit.Framework

[<TestFixture>]
type ParserTests () =
    
    [<TestCase("1")>]
    member this.TestTNum(test: string) =
        Assert.AreEqual(test, 3)
