namespace Euler

module Digits =
    
    open System.Numerics

    let count = BigInteger.Log10 >> ceil >> int

    let ofNumber n = 
        let multiplier = 10I ** (n |> count |> (-) <| 1)
        Seq.unfold (fun (x, factor) -> if x <> 0I then Some(x / factor, (x % factor, factor / 10I)) else None) (n, multiplier)

    let sum = ofNumber >> Seq.sum >> int