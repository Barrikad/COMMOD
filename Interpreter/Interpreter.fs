module Interpreter

open ProgramGraph
open Types
open Actions

exception ErrorInPGException of string

let getBranches progGraph node = 
    let hasRightStartNode node edge = 
        match edge with
              | (start,_,_) when start = node -> true
              | _ -> false
    List.ofSeq (Set.filter (hasRightStartNode node) progGraph)

let traversePG progGraph memInit =
    let rec jumpNode curNode edges mem = 
        match edges with
            | edge::edgesLeft -> match edge with
                                      | (_,BoolAct(boolean),nextNode) when execBool mem boolean -> (curNode,mem) :: 
                                                                                                   jumpNode nextNode (getBranches progGraph nextNode) mem 
                                      | (_,CommandAct(com),nextNode) -> (curNode,mem) :: 
                                                                        jumpNode nextNode (getBranches progGraph nextNode) (execCom mem com)
                                      | _ -> jumpNode curNode edgesLeft mem
            | [] -> [(curNode,mem)]
    
    jumpNode StartNode (getBranches progGraph StartNode) memInit

let rec private traverseEdges config newConfigs edges = 
    match edges with
    | edge::xs -> match edge with
                  | (_,BoolAct(boolean),nextNode) when execBool (snd config) boolean -> traverseEdges config ((nextNode,snd config) :: newConfigs) xs
                  | (_,CommandAct(com),nextNode) -> traverseEdges config ((nextNode, execCom (snd config) com) :: newConfigs) xs
                  | _ -> traverseEdges config newConfigs xs
    | [] -> match newConfigs with
            | [] -> [config]
            | x -> x

let private jumpConfig config pg = traverseEdges config [] (getBranches pg (fst config))
                                   

let rec private traverseConfigs oldConfigs newConfigs stuck pg = 
        match oldConfigs with
        | config::xs -> let newConfig = jumpConfig config pg
                        if List.length newConfig = 1 && newConfig.[0] = config then
                            traverseConfigs xs (newConfig@newConfigs) stuck pg
                        else
                            traverseConfigs xs (newConfig@newConfigs) false pg
        | [] -> if not stuck then
                    traverseConfigs newConfigs [] true pg
                else
                    newConfigs

let endConfigurations progGraph memInit = traverseConfigs [(StartNode,memInit)] [] true progGraph