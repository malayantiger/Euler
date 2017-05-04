namespace Euler

module Tests =

    open NUnit.Framework
    open FsUnit

    [<Test>]
    let ``Problem 01 - Multiple of 3 and 5`` () =
        Number.infiniteSeq
        |> Number.sumMultiplesOf3Or5Below 1000
        |> should equal 233168
    
    [<Test>]
    let ``Problem 02 - Even Fibonacci numbers`` () =
        Fibonacci.infiniteSeq
        |> Seq.takeWhile (fun x  -> x < 4000000I)
        |> Seq.filter (Number.isDivisibleBy 2I)
        |> Seq.sum
        |> should equal 4613732I
    
    [<Test>]
    let ``Problem 05 - Smallest multiple`` () =
        Number.natInfiniteSeq
        |> Seq.find (Number.isDivisibleBy2 [2..20])
        |> should equal 232792560

    [<Test>]
    let ``Problem 06 - Sum square difference`` () =
        Number.natInfiniteSeq
        |> Seq.take 100
        |> Number.sumSquareDiff
        |> should equal 25164150

    [<Test>]
    let ``Problem 16 - Power digit sum`` () =
        2I ** 1000
        |> Digit.sum
        |> should equal 1366

    [<Test>]
    let ``Problem 17 - Number letter counts`` () =
        Number.thousandWords
        |> List.map snd
        |> List.map Text.countLetters
        |> List.reduce (+)
        |> should equal 21124

    [<Test>]
    let ``Problem 20 - Factorial digit sum`` () = 
        100
        |> Factorial.value
        |> Digit.sum
        |> should equal 648

    [<Test>]
    let ``Problem 25 - 1000-digit Fibonacci number`` () = 
        Fibonacci.infiniteSeq
        |> Fibonacci.findTermWithNDigits 1000 
        |> should equal 4782