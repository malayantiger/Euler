namespace Euler

module Fibonacci =

    let recursiveSeq = 
        let rec fib' (n1, n2) = seq {
            yield n1
            yield! fib' (n2, n1+n2)
        }
        fib' (0, 1)

    let seq = Seq.unfold (fun (n1, n2) -> Some(n2, (n2, n1 + n2))) (0I, 1I)

    let value n = seq |> Seq.take n |> Seq.last