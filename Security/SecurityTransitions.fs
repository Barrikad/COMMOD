module SecurityTransitions

open Types
open InitSecurity
open ProgramGraph
open Interpreter

let rec private freeVarsArtm artm = 
    match artm with
    | Int x          -> Set.empty
    | Var str        -> Set.add str Set.empty
    | Arr(str, exp)  -> Set.add str (freeVarsArtm exp)
    | Plus (x1, x2)  -> Set.union (freeVarsArtm x1) (freeVarsArtm x2)
    | Minus (x1, x2) -> Set.union (freeVarsArtm x1) (freeVarsArtm x2)
    | Times (x1, x2) -> Set.union (freeVarsArtm x1) (freeVarsArtm x2)
    | Div (x1, x2)   -> Set.union (freeVarsArtm x1) (freeVarsArtm x2)
    | Neg x          -> freeVarsArtm x
    | Pow (x1, x2)   -> Set.union (freeVarsArtm x1) (freeVarsArtm x2)
    | AError _ -> raise (ErrorInPGException("Can not interpret erroneous PG"))

let rec private freeVarsBool boolean =
    match boolean with
    | True ->  Set.empty 
    | False ->  Set.empty
    | And (a, b) -> Set.union (freeVarsBool a) (freeVarsBool b)
    | Or (a, b) -> Set.union (freeVarsBool a) (freeVarsBool b)
    | AndSC (a, b) -> Set.union (freeVarsBool a) (freeVarsBool b)
    | OrSC (a, b) -> Set.union (freeVarsBool a) (freeVarsBool b)
    | Not (a) -> freeVarsBool a
    | Equal (a, b) -> Set.union (freeVarsArtm a) (freeVarsArtm b)
    | Greater (a, b) -> Set.union (freeVarsArtm a) (freeVarsArtm b)
    | Lesser (a, b) -> Set.union (freeVarsArtm a) (freeVarsArtm b)
    | BError _ -> raise (ErrorInPGException("Can not interpret erroneous PG"))

let private addSetFlows map set var = Set.fold (fun flows var1 -> mapAdd flows (var1,var)) map set

let private addArtmFlows map var artm = addSetFlows map (freeVarsArtm artm) var

let rec flowsCom flows impls command = 
    match command with
    | Assign(var,artm) -> addSetFlows (addArtmFlows flows var artm) impls var
    | ArrAssign(var,artm1,artm2) -> addSetFlows (addArtmFlows (addArtmFlows flows var artm1) var artm2) impls var
    | Skip -> flows
    | Coms(com1,com2) -> flowsCom (flowsCom flows impls com1) impls com2
    | If(gcom) -> flowsGCom flows impls gcom
    | Do(gcom) -> flowsGCom flows impls gcom 
    | CError _ -> raise (ErrorInPGException("Can not interpret erroneous PG"))

and flowsGCom flows impls gcommand =
    match gcommand with
    | Gives(bol,com) -> flowsCom flows (Set.union impls (freeVarsBool bol)) com
    | Else(gcom1,gcom2) -> flowsGCom (flowsGCom flows impls gcom1) (Set.union impls (freeVarsBool (doneGCommand gcom1))) gcom2
    | GError _ -> raise (ErrorInPGException("Can not interpret erroneous PG"))

let private addAction (flows,impl) act =
    match act with
    | BoolAct(bol) -> (flows,Set.union impl (freeVarsBool bol))
    | CommandAct(com) -> (flowsCom flows impl com,impl)


let flowsInPG pg = 
    let mutable flows = Map.empty
    let mutable visited = Set.empty
    let rec flowJump (sNode,act,eNode) impls =  visited <- Set.add (sNode,act,eNode) visited
                                                let edges = Set.difference (Set.ofList (getBranches pg eNode)) visited
                                                let fwip = addAction (flows,impls) act
                                                flows <- fst fwip
                                                Set.fold (fun () e -> flowJump e (snd fwip)) () edges
    List.fold (fun () e -> flowJump e Set.empty) () (getBranches pg StartNode)
    flows