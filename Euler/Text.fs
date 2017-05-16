namespace Euler

module Text =
    
    open System.Text.RegularExpressions

    let countLetters s = s |> (Seq.filter (fun c -> c <> ' ') >> Seq.length)

    let stripWhitespaces s = Regex.Replace(s, @"\s+", "")

    let scores (xs: string seq) = 
        let alphabet = ['A'..'Z'] |> Seq.mapi (fun s l -> (l, s+1)) |> dict
        xs |> Seq.mapi (fun i x -> (i+1) * Seq.fold (fun s l -> s + alphabet.Item(l)) 0 x)
