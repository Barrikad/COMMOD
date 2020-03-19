module progGraph

open TypeAST

exception ErrorInTree of string

type Node = 
    | StartNode
    | EndNode
    | MidNode of int

type Action =
    | BoolAct of Boolean
    | CommandAct of Command
    | GCommandAct of GCommand

let rec doneGCommand gcom =
    match gcom with
          | Gives(bol,_) -> Not(bol)
          | Else(gcom1,gcom2) -> And(doneGCommand gcom1, doneGCommand gcom2)
          | GError(_) -> raise (ErrorInTree("Can not construct GP with erroneous guarded commands"))

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
              | CError(_) -> raise (ErrorInTree("Can not construct GP with erroneous commands"))

    and expandGCommand qs gcom qe = 
        match gcom with
              | Gives(bol,com) -> let qm  = MidNode(nextFreeNode 0)
                                  Set.add (qs,BoolAct(bol),qm) (expandCommand qm com qe)
              | Else(gcom1,gcom2) -> if deterministic then
                                        let qm = MidNode(nextFreeNode 0)
                                        Set.union (expandGCommand qs gcom1 qe) (Set.union (Set.add (qs,BoolAct(doneGCommand gcom1),qm) Set.empty) (expandGCommand qm gcom2 qe))
                                     else
                                        Set.union (expandGCommand qs gcom1 qe) (expandGCommand qs gcom2 qe)
              | GError(_) -> raise (ErrorInTree("Can not construct GP with erroneous guarded commands"))

    expandCommand StartNode ast EndNode
    
    
    
    
    (*let rec guardedCommands currNode nextNode out = function
        | Gives(guard, command) -> let x = appendNode currNode
                                   let y = Set.add (currNode, BoolAct(guard), x) out
                                   commands x nextNode y command
        | Else(gcom1,gcom2) -> let z = guardedCommands currNode nextNode out gcom1
                               guardedCommands currNode nextNode z gcom2

    and commands currNode nextNode out = function
        | Assign(str, arithm) -> Set.add (currNode, CommandAct(Assign(str,arithm)), nextNode) out
        | ArrAssign(str,arithm1,arithm2) -> Set.add(currNode,CommandAct(ArrAssign(str,arithm1,arithm2)),nextNode) out
        | Skip -> Set.add(currNode,CommandAct(Skip),nextNode) out
        | Coms(command1, command2) -> let x = appendNode currNode
                                      let holder = commands currNode x out command1
                                      commands x nextNode holder command2
        | If(gcom) -> guardedCommands currNode nextNode out gcom
        | Do(gcom)-> match gcom with
                           | Gives(bool,com) -> let holder = guardedCommands currNode currNode out gcom
                                                Set.add (currNode,BoolAct(Not bool),nextNode) holder*)
               