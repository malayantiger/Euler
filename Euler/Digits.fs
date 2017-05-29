namespace Euler

module Digits =
    
    open System.Numerics

    let count = BigInteger.Log10 >> ceil >> int

    let ofNumber = Seq.unfold (fun b -> if b <> 0I then Some(b % 10I, (b / 10I)) else None) >> List.ofSeq >> List.rev

    let sum = ofNumber >> Seq.sum >> int

    let groupAdjacent count digits = 
        seq {
            for (i, d) in List.indexed digits do
                if i + count < List.length digits then
                    yield digits |> List.skip i |> List.take count
        }
        