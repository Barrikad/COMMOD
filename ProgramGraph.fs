module ProgramGraph

open Types

exception ErrorInTreeException of string


let rec doneGCommand gcom =
    match gcom with
          | Gives(bol,_) -> Not(bol)
          | Else(gcom1,gcom2) -> AndSC(doneGCommand gcom1, doneGCommand gcom2)
          | GError(_) -> raise (ErrorInTreeException("Can not construct PG with erroneous guarded commands"))

let AST2PG ast deterministic =
    let mutable highestNode = 0
    let nextFreeNode x = highestNode <- highestNode + 1
                         highestNode    
    let rec expandCommand qs com qe =
        match com with
              | Assign(str,artm) -> Set.empty.Add (qs,CommandAct(Assign(str,artm)),qe)
              | ArrAssign(str,artm1,artm2) -> Set.add (qs,CommandAct(ArrAssign(str,artm1,artm2)),qe) Set.empty
              | Skip -> Set.empty.Add (qs,CommandAct(Skip),qe)
              | Coms(com1,com2) -> let qm = MidNode(nextFreeNode 0)
                                   Set.union (expandCommand qs com1 qm) (expandCommand qm com2 qe)
              | If(gcom) -> Set.union (expandGCommand qs gcom qe) (Set.add (qs,BoolAct(doneGCommand gcom),qe) Set.empty)
              | Do(gcom) -> Set.union (expandGCommand qs gcom qs) (Set.add (qs,BoolAct(doneGCommand gcom),qe) Set.empty)
              | CError(_) -> raise (ErrorInTreeException("Can not construct PG with erroneous commands"))
    and expandGCommand qs gcom qe = 
        match gcom with
              | Gives(bol,com) -> let qm  = MidNode(nextFreeNode 0)
                                  Set.add (qs,BoolAct(bol),qm) (expandCommand qm com qe)
              | Else(gcom1,gcom2) -> if deterministic then
                                        let qm = MidNode(nextFreeNode 0)
                                        Set.union (expandGCommand qs gcom1 qe) (Set.union (Set.add (qs,BoolAct(doneGCommand gcom1),qm) Set.empty) (expandGCommand qm gcom2 qe))
                                     else
                                        Set.union (expandGCommand qs gcom1 qe) (expandGCommand qs gcom2 qe)
              | GError(_) -> raise (ErrorInTreeException("Can not construct PG with erroneous guarded commands"))

    expandCommand StartNode ast EndNode
               