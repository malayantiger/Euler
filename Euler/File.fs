namespace Euler

module File' =

    open System.IO

    let readInput problemNumber = __SOURCE_DIRECTORY__ + "\problem" + problemNumber.ToString() + "Input.txt" |> File.ReadAllLines