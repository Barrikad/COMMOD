module Interpreter

open GCPProgramGraph
open ASTType
open GCPExecution

exception ErrorInPGException of string

let traversePG progGraph memInit =
    let hasRightStartNode node edge = 
        match edge with
              | (start,_,_) when start = node -> true
              | _ -> false

    let getBranches node = List.ofSeq (Set.filter (hasRightStartNode node) progGraph)

    let rec jumpNode curNode edges mem = 
        match edges with
            | edge::edgesLeft -> match edge with
                                      | (_,BoolAct(boolean),nextNode) when execBool mem boolean -> (curNode,mem) :: jumpNode nextNode (getBranches nextNode) mem 
                                      | (_,CommandAct(com),nextNode) -> (curNode,mem) :: jumpNode nextNode (getBranches nextNode) (execCom mem com)
                                      | _ -> jumpNode curNode edgesLeft mem
            | [] -> [(curNode,mem)]
    
    jumpNode StartNode (getBranches StartNode) memInit
