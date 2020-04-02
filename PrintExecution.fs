module PrintExecution

open System
open Types

let printMem mem =
    let valFold _ str value = printfn "%s: %i" str value
    let arrFold _ str list = 
        let rec listPrint xs int =
            match xs with
            | y::ys -> printfn "%s[%i]: %i" str int y
                       listPrint ys (int + 1)
            | [] -> ()
        listPrint list 0
    let printValMem mem = Map.fold valFold () mem
    let printArrMem mem = Map.fold arrFold () mem

    printValMem (fst mem)
    printArrMem (snd mem)

let rec printExec configs =
    match configs with
    | [(q,mem)] -> let node = match q with
                              | StartNode -> "q▷"
                              | EndNode -> "q◀"
                              | MidNode x -> sprintf "q%i" x
                   let status = if q = EndNode then "terminated" else "stuck"
                   printfn "status: %s" status
                   printfn "Node: %s" node
                   printMem mem
                   printfn ""
    | (q,mem)::xs -> let node = match q with
                                | StartNode -> "q▷"
                                | EndNode -> "q◀"
                                | MidNode x -> sprintf "q%i" x
                     let status = "running"
                     printfn "status: %s" status
                     printfn "Node: %s" node
                     printMem mem
                     printfn ""
                     printExec xs
    | [] -> ()