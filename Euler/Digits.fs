namespace Euler

module Digits =
    
    open System.Numerics

    let count = BigInteger.Log10 >> ceil >> int

    let ofNumber = Seq.unfold (fun b -> if b <> 0I then Some(b % 10I, (b / 10I)) else None) >> List.ofSeq >> List.rev

    let sum = ofNumber >> Seq.sum >> int