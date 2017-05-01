[<EntryPoint>]
let main argv = 
    Euler.Number.thousandWords |> List.iter (printf "%A\n") |> ignore
    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
