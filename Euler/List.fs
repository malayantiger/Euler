namespace Euler

module List' =

    let equalPartition xs = xs |> List.splitAt (xs.Length / 2)
