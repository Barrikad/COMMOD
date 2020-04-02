module Interpreter

open ProgramGraph
open Types
open Execution

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
