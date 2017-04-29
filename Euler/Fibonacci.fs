namespace Euler

module Fibonacci =

    let recursiveInfiniteSeq = 
        let rec fib' (n1, n2) = seq {
            yield n1
            yield! fib' (n2, n1+n2)
        }
        fib' (0, 1)

    let infiniteSeq = Seq.unfold (fun (n1, n2) -> Some(n2, (n2, n1 + n2))) (0I, 1I)

    let value n = infiniteSeq |> Seq.take n |> Seq.last

    let findTermWithNDigits n = n |> Number.findIndexWithNDigits >> ((+) 1)