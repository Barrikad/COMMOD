module InitSecurity

open System

let latticeStrings = IO.File.ReadAllLines "Input\\lattice.txt"
let classStrings = IO.File.ReadAllLines "Input\\classification.txt"


let private getLevels (str:string) = let clean = Seq.filter (fun s -> not (s="")) ((str.Replace("<"," ")).Split ' ')
                                     (Seq.item 0 clean, Seq.item 1 clean)

let private getClass (str:string) = let clean = Seq.filter (fun s -> not (s="")) ((str.Replace("="," ")).Split ' ')
                                    (Seq.item 1 clean, Seq.item 0 clean)

let mapAdd map (str1,str2) = let oldValue = Map.tryFind str1 map
                             match oldValue with
                             | Some v -> Map.add str1 (Set.add str2 v) map
                             | None -> Map.add str1 (Set.add str2 Set.empty) map


let tryFindSet key map = 
    match Map.tryFind key map with
    | Some s -> s
    | None -> Set.empty

let rec private allDescendants map key = 
    match Map.tryFind key map with
    | None -> Set.empty
    | Some s -> Set.union s (Set.fold (fun setO v -> Set.union setO (allDescendants map v)) Set.empty s)

let private mapClosure oldMap newMap key _ = Map.add key (Set.add key (allDescendants oldMap key)) newMap

let private securityLatticeT : Map<string,Set<string>> = Seq.fold (fun map str -> mapAdd map (getLevels str)) Map.empty latticeStrings
let securityLattice = Map.fold (mapClosure securityLatticeT) Map.empty securityLatticeT
let variableClassification  : Map<string,Set<string>> = Seq.fold (fun map str -> mapAdd map (getClass str)) Map.empty classStrings
