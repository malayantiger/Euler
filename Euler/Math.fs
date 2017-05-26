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

    let isPrimeBig n = 
        if n % 2I = 0I 
            then false
        else
            seq { 3I .. 2I .. bigint(sqrtn n) }
            |> Seq.tryFind (fun x -> n % x = 0I)
            |> function
                | Some(x) -> false
                | _ -> true
        
    let coPrimes n = 
        positiveIntegers 
        |> Seq.truncate (n-1) 
        |> Seq.filter (fun x -> isCoPrime n x)
    
    let divisors (n: bigint) =
        let divisors = 
            seq { 2I .. bigint(sqrtn n) }
            |> Seq.filter (fun x -> n % x = 0I)
        seq {
            yield! divisors
            yield! divisors |> Seq.map (fun x -> n / x)
        }

    let primeFactors = divisors >> Seq.filter (fun x -> isPrimeBig x)

    let sigma n =
        positiveIntegers
        |> Seq.truncate (sqrtn n)
        |> Seq.fold (fun s x -> if n % x = 0 then s + x + n / x else s) 0
        |> (-) <| n

    let sumOfSquares xs = Seq.fold (fun s x -> s + x * x) 0 xs

    let squareOfSums xs = (xs |> Seq.sum) |> pown <| 2

    let sumSquareDiff xs = (squareOfSums xs) - (sumOfSquares xs)