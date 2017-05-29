namespace Euler

module File' =

    open System.IO

    let readInput read problemNumber = 
        let fileNumber = if problemNumber  < 10 then "0" + problemNumber.ToString() else problemNumber.ToString()
        __SOURCE_DIRECTORY__ + "\problem" + fileNumber + "Input.txt" |> read

    let readInputLines = readInput File.ReadAllLines

    let readInputText = readInput File.ReadAllText

    let readInputAsLine = readInputLines >> Seq.fold (+) ""