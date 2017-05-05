namespace Euler

module List' =

    let equalPartition xs =
        let rec equalPartition' = function
            | xs1, xs2, ([] | [_]) -> (xs1 |> List.rev, xs2)
            | _, [], _ -> ([], [])
            | xs1, x::xs, y::y'::ys -> equalPartition' (List.Cons(x, xs1), xs, ys)
        equalPartition' ([], xs, xs)
