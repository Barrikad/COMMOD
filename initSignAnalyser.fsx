open System 

type Sign = Positive | Negative | Zero

let readLines filePath = System.IO.File.ReadLines filePath |> Set.ofSeq

readLines "C:/Users/320/source/repos/ComputerScienceModelling/Version1LexPars/COMMOD-master/initVarMem.txt"

let parSign = Array.map (fun elem -> match elem with
                                     | "+" -> Positive
                                     | "-" -> Negative
                                     | "0" -> Zero)

let parseWhite (str:string) = str.Split ' '
let parse1 (str:string) = str.Replace(" := ", " ")
let parse2 (str:string) = str.Replace("{", "")
let parse3 (str:string) = str.Replace("}", "")
let parse4 (str:string) = str.Replace(",", " ")

let ah = Array.head
let at = Array.tail

let parseSet = Set.map(fun x -> (parseWhite(parse1(parse2(parse3(parse4(x))))))) >> Set.map(fun x -> ah x) >> Set.toList
let parseSet1 = Set.map(fun x -> (parseWhite(parse1(parse2(parse3(parse4(x))))))) >> Set.map(fun x -> parSign(at x)) >> Set.toList

let p1 = parseSet (readLines("C:/Users/320/source/repos/ComputerScienceModelling/Version1LexPars/COMMOD-master/initVarMem.txt"))
let p2 = parseSet1 (readLines("C:/Users/320/source/repos/ComputerScienceModelling/Version1LexPars/COMMOD-master/initVarMem.txt"))

let rec merge (p1:string list, p2:Sign[] list) = Map.ofList(List.zip p1 p2)


merge (p1, p2)
                    
            


