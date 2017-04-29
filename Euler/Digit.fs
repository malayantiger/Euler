namespace Euler

module Digit =

    let seq = Seq.unfold (fun b -> if b <> 0I then Some(b % 10I, (b / 10I)) else None)

    let count = seq >> Seq.length >> int

    let sum = seq >> Seq.sum >> int