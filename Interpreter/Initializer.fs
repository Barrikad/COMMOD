module Initializer

open System
open Types

let private initMemStrings = IO.File.ReadAllLines "Input\\initialMemory.txt"

let private arrStrings = Seq.toList (Seq.filter (Seq.contains '[') initMemStrings)
let private varStrings = Seq.toList (Seq.filter (fun t -> not (Seq.contains t arrStrings)) initMemStrings)

let rec private parseVarStrings strings varMem =
    match strings with
    | (string:String) :: xs -> let split = Seq.filter (fun s -> not (s = "")) (string.Replace("="," ").Split ' ')
                               let newMem = Map.add (Seq.head split) ((Seq.last split) |> int) varMem
                               parseVarStrings xs newMem
    | [] -> varMem

let rec private listInts ints =
    match ints with
    | x::xs -> (x |> int) :: listInts xs
    | [] -> []
    
let rec private parseArrStrings strings arrMem =
    match strings with
    | (string:String) :: xs -> let cleaned = string.Replace("="," ").Replace("[","").Replace("]","").Replace(","," ")
                               let split = Seq.filter (fun s -> not (s = "")) (cleaned.Split ' ')
                               let newMem = match Seq.toList split with
                                            | y::ys -> Map.add y (listInts ys) arrMem
                                            | [] -> failwith "malformed initial memory"
                               parseArrStrings xs newMem
    | [] -> arrMem

let initialMemory = (parseVarStrings varStrings Map.empty, parseArrStrings arrStrings Map.empty)