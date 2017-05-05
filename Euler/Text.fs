namespace Euler

module Text =
    
    open System.Text.RegularExpressions

    let countLetters s = s |> (Seq.filter (fun c -> c <> ' ') >> Seq.length)

    let stripWhitespaces s = Regex.Replace(s, @"\s+", "")
