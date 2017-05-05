namespace Euler

module Digit =
    
    open System.Numerics

    let count n = ceil(BigInteger.Log10(n)) |> int

    let seq n = 
        let factorPower = (count n) - 1
        Seq.unfold (fun (x, factor) -> if x <> 0I then Some(x / factor, (x % factor, factor / 10I)) else None) (n, 10I ** factorPower)

    let toNumber xs = 
        let factorPower = 10I ** (xs |> Seq.length |> (-) <| 1)
        xs 
        |> Seq.fold (fun (n, factor) x -> (n + x * factor, factor / 10I)) (0I, factorPower)
        |> fst

    let sum = seq >> Seq.sum >> int