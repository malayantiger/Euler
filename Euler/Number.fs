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

    let singleDigitsWords = seq [(1, "one"); (2, "two"); (3, "three"); (4, "four"); (5, "five"); (6, "six"); (7, "seven"); (8, "eight"); (9, "nine")]

    let twoDigitsWords = seq [(10, "ten"); (11, "eleven"); (12, "twelve"); (13, "thirteen"); (14, "fourteen"); (15, "fifteen"); (16, "sixteen"); (17, "seventeen"); (18, "eighteen"); (19, "nineteen")]

    let tensMultipleWords = seq [(20, "twenty"); (30, "thirty"); (40, "forty"); (50, "fifty"); (60, "sixty"); (70, "seventy"); (80, "eighty"); (90, "ninety")]

    let hundredWord = [(100, "hundred")]

    let thousandWord = [(1000, "one thousand")]

    let thousandWords = 
        let combineNumbers selector1 selector2 xs ys =
            seq { 
                for (x1, x2) in xs do 
                for (y1, y2) in ys do 
                    yield (selector1 x1 y1, selector2 x2 y2)
            }

        let concatWithSpace x y = x + " " + y

        let tensWords = 
            seq { 
                yield! tensMultipleWords
                yield! combineNumbers (+) concatWithSpace tensMultipleWords singleDigitsWords
            }
        
        let hundredsMultipleWords = combineNumbers (*) concatWithSpace singleDigitsWords hundredWord

        let hundredsWords = 
            seq { 
                yield! hundredsMultipleWords 
                yield! combineNumbers (+) (fun x y -> x + " and " + y) hundredsMultipleWords (seq [singleDigitsWords; twoDigitsWords; tensWords] |> Seq.concat)
            }

        seq {
            yield! singleDigitsWords 
            yield! twoDigitsWords 
            yield! tensWords
            yield! hundredsWords
            yield! thousandWord
        } 
        |> Seq.sortBy snd
        |> List.ofSeq
        