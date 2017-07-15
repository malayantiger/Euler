namespace Euler

module Math =

    let integers = Seq.initInfinite id

    let positiveIntegers = integers |> Seq.skip 1

    let inline sqrtn n = n |> (float >> sqrt >> floor >> int)

    let gcd a b = 
        let rec gcd' a b =
            match b with 
            | 0 -> a
            | _ -> gcd' b (a % b)
        gcd' a b
    
    let gcd2 ns = Seq.reduce gcd ns

    let lcm a b = (abs a) / (gcd a b) * b

    let lcm2 ns = Seq.reduce lcm ns

    let isCoPrime n x = gcd n x = 1

    let isPrime = function
        | n when n = 2I || n = 3I -> true
        | n when n > 3I && (n % 2I = 0I || n % 3I = 0I) -> false
        | n ->
            seq { 5I .. 2I .. bigint(sqrtn n) }
            |> Seq.exists (fun x -> n % x = 0I)
            |> not
        
    let coPrimes n = 
        positiveIntegers 
        |> Seq.truncate (n-1) 
        |> Seq.filter (fun x -> isCoPrime n x)
    
    let divisors n =
        let divisors = 
            seq { 2I .. bigint(sqrtn n) }
            |> Seq.filter (fun x -> n % x = 0I)
        seq {
            yield! divisors
            yield! divisors |> Seq.map (fun x -> n / x)
        } |> Seq.toList

    let primesUpTo n =
        let sieve = seq { 2I .. bigint(sqrtn n) } |> Seq.filter isPrime |> Seq.toList
        seq {
            for x in 2I..n do
                if not (List.exists (fun y -> x % y = 0I && x <> y) sieve) then
                    yield x
        }

    let primeFactors = divisors >> List.filter isPrime

    let sigma n =
        positiveIntegers
        |> Seq.truncate (sqrtn n)
        |> Seq.fold (fun s x -> if n % x = 0 then s + x + n / x else s) 0
        |> (-) <| n

    let sumOfSquares xs = Seq.fold (fun s x -> s + x * x) 0 xs

    let squareOfSums xs = (xs |> Seq.sum) |> pown <| 2

    let sumSquareDiff xs = (squareOfSums xs) - (sumOfSquares xs)

    let pythagoreanTriplets = 
        let fib = Fibonacci.seq |> Seq.cache
        let fibNth n = fib |> Seq.item n
        Seq.unfold (fun (a, b, c, n) -> Some((a, b, c), (a+b+c, (fibNth (2*n-1)) - b, (fibNth (2*n)), n+1))) (4I, 3I, 5I, 3)

    let combinations n k = Factorial.value n / (Factorial.value k * Factorial.value (n-k)) 