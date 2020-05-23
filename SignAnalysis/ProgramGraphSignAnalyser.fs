module PGSigns

open SignAnalyser
open Interpreter
open Types

let signsInPG initialMemory pg =
    let mutable signMap = Map.empty
    let rec insertInMap (node,memSet1) =
        if Map.count signMap > 10 then false else
        match Map.tryFind node signMap with
            | None -> signMap <- Map.add node memSet1 signMap
                      true
            | Some memSet2 -> signMap <- Map.add node (Set.union memSet1 memSet2) signMap
                              not (Set.isSubset memSet1 memSet2)

    let rec jumpNode edgeToNode absMem =
        if Set.count absMem = 0 then () else
        match edgeToNode with
        | (_,BoolAct(bool),nextNode) -> let newMem = Set.filter (fun mem -> Set.contains true (booleanSign mem bool)) absMem
                                        match nextNode with
                                        | EndNode -> ignore (insertInMap (EndNode,newMem))
                                        | _ -> if insertInMap (nextNode,newMem) then traverseEdges newMem (getBranches pg nextNode) else ()
                                           
        | (_,CommandAct(com),nextNode) -> let newMem = Set.fold (fun memSet mem -> Set.union memSet (comSign mem com)) Set.empty absMem
                                          match nextNode with
                                          | EndNode -> ignore (insertInMap (EndNode,newMem))
                                          | _ -> if insertInMap (nextNode,newMem) then traverseEdges newMem (getBranches pg nextNode) else ()
    and traverseEdges mem edges =
        match edges with
        | edge::restEdges -> jumpNode edge mem
                             traverseEdges mem restEdges
        | [] -> ()

    jumpNode (StartNode,BoolAct(True),StartNode) (Set.empty.Add initialMemory)
    signMap