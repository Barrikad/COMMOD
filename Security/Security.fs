module Security

open Types
open InitSecurity
open SecurityTransitions


let private addFlow flows setX setY = Set.fold (fun map1 s1 -> (Set.fold (fun map2 s2 -> mapAdd map2 (s1,s2)) map1 setY)) flows setX

let private extractVars key = match Map.tryFind key variableClassification with
                              | Some s -> s
                              | None -> Set.empty

let private findFlows flows latticeKey latticeValues = let keySet = extractVars latticeKey
                                                       let valueSet = Set.fold (fun vars key -> Set.union vars (extractVars key)) Set.empty latticeValues
                                                       addFlow flows keySet valueSet

let allowedFlows = (Map.fold findFlows Map.empty securityLattice) 

let violates vs key value = Set.fold (fun s v -> if (Set.contains v (tryFindSet key allowedFlows))
                                                 then s else Set.add (key,v) s) vs value

let violations actualFlows = Map.fold violates Set.empty actualFlows