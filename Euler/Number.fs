namespace Euler

module Number = 

    let infiniteSeq = Seq.initInfinite id

    let natInfiniteSeq = infiniteSeq |> Seq.skip 1

    let hasNDigits n x = (Digit.count x) = n

    let findWithNDigits n = Seq.find (hasNDigits n)

    let findIndexWithNDigits n = Seq.findIndex (hasNDigits n)

    let isMultipleOf3Or5 x = x % 3 = 0 || x % 5 = 0

    let sumMultiplesOf3Or5Below n = n |> Seq.take >> Seq.filter isMultipleOf3Or5 >> Seq.sum

    let sumOfSquares xs = Seq.fold (fun s x -> s + x*x) 0 xs

    let squareOfSums xs = pown (Seq.sum xs) 2

    let sumSquareDiff xs = (squareOfSums xs) - (sumOfSquares xs)