module InitSigns

open Types

let readLines filePath = System.IO.File.ReadLines filePath |> Set.ofSeq

let lines = readLines "Input\\initialSigns.txt"

let arrLines = Set.filter (fun str -> Seq.exists (fun cha -> cha = '{') str) lines
let varLines = Set.filter (fun str -> not (Seq.exists (fun cha -> cha = '{') str)) lines

let parSign elem = 
    match elem with
    | "+" -> Positive
    | "-" -> Negative
    | "0" -> Zero
    | _ -> failwith "Undefined sign"

let parseWhite (str:string) = str.Split ' '
let cleanString (str:string) = str.Replace(" := ", " ").Replace("{", "").Replace("}", "").Replace(",", " ")

let ah = Array.head
let at = Array.tail

let getKeyAndValueArr strList = match strList with
                                | head :: rest -> let key = head
                                                  let value = List.fold (fun set str -> Set.add (parSign str) set) Set.empty rest
                                                  (key,ArrSign value)
                                | [] -> failwith "Misformed initial signs"

let getKeyAndValueVal strList = let key = List.head strList
                                let value = strList.Item 1
                                (key,VarSign (parSign value))

let arrMem = Set.fold (fun list line -> (getKeyAndValueArr (List.ofSeq (parseWhite (cleanString line))))::list) [] arrLines
let varMem = Set.fold (fun list line -> (getKeyAndValueVal (List.ofSeq (parseWhite (cleanString line))))::list) [] varLines

let getInit = Map.ofList (arrMem @ varMem)
   