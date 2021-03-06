﻿namespace Euler

module Number = 

    let ofDigits xs = 
        let multiplier = 10I ** (xs |> Seq.length |> (-) <| 1)
        xs 
        |> Seq.fold (fun (n, factor) x -> (n + x * factor, factor / 10I)) (0I, multiplier)
        |> fst

    let hasNDigits n x = x |> Digits.count = n

    let findWithNDigits n = n |> hasNDigits |> Seq.find

    let findIndexWithNDigits n = Seq.findIndex (hasNDigits n)

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

    let reverse n =
        let rec reverse' n' rev =
            let digit = n' % 10I
            if n' = 0I then rev
            else reverse' (n' / 10I) (rev * 10I + digit)
        reverse' n 0I

    let isPalindrome n = n = reverse n

    let isAmicable n = 
        let dn = Math.sigma n
        n <> dn && n = Math.sigma dn