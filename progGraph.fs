module progGraph

open TypeAST

let mutable setOfNodes = Set.empty 

type nodes = 
    | Start
    | End
    | Node of int

type Action =
    | BoolAct of Boolean
    | ArithmAct of Arithm
    | CommandAct of Command
    | GCommandAct of GCommand

let rec appendNode node = 
    match node with
    | Start -> Node(0)
    | End -> failwith("Impossible to append to final node") 
    | Node n -> if Set.contains (n+1) setOfNodes then
                    appendNode (Node(n+1))
                    else
                    setOfNodes <- Set.add (n+1) setOfNodes
                    Node(n+1)

let rec guardedCommands currNode nextNode out = function
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
    | Do(gcom)-> let (bool,com) = gcom
                 let holder = guardedCommands currNode currNode out gcom
                 Set.add (currNode,BoolAct(Not bool),nextNode) holder
               