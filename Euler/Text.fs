namespace Euler

module Text =
    
    let countLetters s = s |> (Seq.filter (fun c -> c <> ' ') >> Seq.length)

