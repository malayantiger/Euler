namespace Euler

module Problems =

    open NUnit.Framework
    open FsUnit
    open System.Numerics
    open FSharp.Collections.ParallelSeq

    [<Test>]
    let ``Problem 01 - Multiple of 3 and 5`` () =
        [1..999]
        |> Seq.filter (fun x -> x % 3 = 0 || x % 5 = 0)
        |> Seq.sum
        |> should equal 233168

    [<Test>]
    let ``Problem 02 - Even Fibonacci numbers`` () =
        Fibonacci.seq
        |> Seq.takeWhile (fun x  -> x < 4000000I)
        |> Seq.filter (fun x -> x % 2I = 0I)
        |> Seq.sum
        |> should equal 4613732I
    
    [<Test>]
    let ``Problem 03 - Largest prime factor`` () =
        600851475143I
        |> Math.primeFactors
        |> Seq.max
        |> should equal 6857I

    [<Test>]
    let ``Problem 04 - Largest palindrome product`` () =
        seq {
            for x in 101I..999I do
            for y in 101I..999I do
                let product = x * y
                if product |> Number.isPalindrome then yield product
        }
        |> Seq.max
        |> should equal 906609I

    [<Test>]
    let ``Problem 05 - Smallest multiple`` () =
        [1..20] 
        |> Math.lcm2
        |> should equal 232792560

    [<Test>]
    let ``Problem 06 - Sum square difference`` () =
        Math.positiveIntegers
        |> Seq.take 100
        |> Math.sumSquareDiff
        |> should equal 25164150
    
    [<Test>]
    let ``Problem 08 - Largest product in a series`` () =
        File'.readInputAsLine 8
        |> BigInteger.Parse
        |> Digits.ofNumber
        |> Digits.groupAdjacent 13
        |> Seq.map (Seq.reduce (*))
        |> Seq.max
        |> should equal 23514624000I
    
    [<Test>]
    let ``Problem 09 - Special Pythagorean triplet`` () =
        let s = 1000
        seq {
            for a in 1..s/3 do
            for b in 1..s/2 do
                let c = s - a - b
                if a*a + b*b = c*c then
                    yield [a; b; c]
        }
        |> Seq.find (Seq.reduce (+) >> (=) s)
        |> Seq.reduce (*)
        |> should equal 31875000

    [<Test>]
    let ``Problem 10 - Summation of primes`` () =
        Math.primesTo' 2000000I
        |> Seq.sum
        |> should equal 31875000

    [<Test>]
    let ``Problem 13 - Large sum`` () =
        File'.readInputLines 13
        |> Seq.map BigInteger.Parse
        |> Seq.reduce (+)
        |> Digits.ofNumber
        |> Seq.take 10
        |> Number.ofDigits
        |> should equal 5537376230I

    [<Test>]
    let ``Problem 16 - Power digit sum`` () =
        2I ** 1000
        |> Digits.sum
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
        |> Digits.sum
        |> should equal 648

    [<Test>]
    let ``Problem 21 - Amicable numbers`` () = 
        [2..9999]
        |> Seq.filter Number.isAmicable
        |> Seq.sum
        |> should equal 31626
    
    [<Test>]
    let ``Problem 22 - Name scores`` () = 
        File'.readInputLines 22
        |> Seq.collect (fun x -> x.Split(','))
        |> Seq.map (fun x -> x.Replace("\"", ""))
        |> Seq.sort
        |> Text.scores
        |> Seq.sum
        |> should equal 871198282

    [<Test>]
    let ``Problem 25 - 1000-digit Fibonacci number`` () = 
        Fibonacci.seq
        |> Number.findIndexWithNDigits (1000)
        |> (+) 1
        |> should equal 4782
