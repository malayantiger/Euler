namespace Euler

module Factorial = 

    let infiniteSeq = Seq.unfold (fun (i, p) -> Some(p, (i + 1I, i * p))) (1I, 1I) 

    let value n = infiniteSeq |> (Seq.take n >> Seq.last)