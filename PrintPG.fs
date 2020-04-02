module PrintPG

open ProgramGraph
open Types

let rec arithm2str (x : Arithm) =
    match x with 
    | Int (x) -> (string x)
    | Float (x) -> (string x)
    | Val (x) -> (string x)
    | ArrVal (a,b) -> a + "[" + arithm2str(b) + "]" 
    | Plus (a,b) -> "(" + arithm2str (a) + "+" + arithm2str (b) + ")"
    | Minus (a,b) -> "(" + arithm2str (a) + "-" + arithm2str (b) + ")"
    | Times (a,b) -> "(" + arithm2str (a) + "*" + arithm2str (b) + ")"
    | Div (a,b) -> "(" + arithm2str (a) + "/" + arithm2str (b) + ")"
    | Neg (x) -> "(-" + arithm2str (x) + ")"
    | ParA (x) -> "(" + arithm2str(x) + ")"
    | Pow (a,b) -> "(" + arithm2str (a) + "^" + arithm2str (b)
    | AError a-> failwith "strange, an AError at this point?"

let rec bool2str (x : Boolean) =
    match x with 
    | True -> "True"
    | False -> "False"
    | And (a,b) -> "(" + bool2str (a) + " & " + bool2str (b) + ")"
    | Or (a,b) -> "(" + bool2str (a) + " | " + bool2str (b) + ")"
    | AndSC (a,b) -> "(" + bool2str (a) + " && " + bool2str (b) + ")"
    | OrSC (a,b) -> "(" + bool2str (a) + " || " + bool2str (b) + ")"
    | Equal (a,b) -> "(" + arithm2str (a) + "==" + arithm2str (b) + ")"
    | Greater (a,b) -> "(" + arithm2str (a) + ">" + arithm2str (b) + ")"
    | Lesser (a,b) -> "(" + arithm2str (a) + "<" + arithm2str (b) + ")"
    | Not (a) -> "!" + bool2str(a) 
    | ParB (x) -> "(" + bool2str(x) + ")"
    | BError a-> failwith "strange, an BError at this point?"

let rec Command2str (x : Command) =
    match x with
    | Assign (a,b) -> a + ":=" + arithm2str(b)
    | ArrAssign (a,b,c) -> a + "[" + arithm2str(b) + "]:=" + arithm2str(c) 
    | Skip -> "skip"
    | Coms (a,b) -> Command2str(a) + " " + Command2str(b)
    | If (x) -> GCommand2str(x)
    | Do (x) -> GCommand2str(x)
    | CError a -> failwith "strange, an CError at this point?"
and GCommand2str (x : GCommand) =
    match x with
    | Gives (a,b) -> "if " + bool2str (a) + " then " + Command2str(b)
    | Else (a,b) -> GCommand2str(a) + " else " +  GCommand2str(b)
    | GError a-> failwith "strange, an GError at this point?"

let act2str = function
    | BoolAct (x) -> bool2str (x)
    | CommandAct (x) -> Command2str (x)

let toDOT (x) =
    match x with
    | (StartNode,b,EndNode) -> 
        "q▷ -> q◀ [label = \"" + act2str(b) + "\"];" 
    | (StartNode,b,MidNode c) ->  
        "q▷ -> q" + string(c) + " [label = \"" + act2str(b) + "\"];" 
    | (MidNode a,b,EndNode) -> 
        "q" + string(a) + " -> q◀ [label = \"" + act2str(b) + "\"];" 
    | (MidNode a,b,MidNode c) ->  
        "q" + string(a) + " -> q" + string(c) + " [label = \"" + act2str(b) + "\"];" 
    | _ -> failwith "Incorrect Edges"

let rec edge2string (l : (Node*Action*Node) list) =
    match l with 
    | x::tail -> toDOT(x) + "\n" + edge2string tail
    | [] -> ""

let toList s = Set.fold (fun l se -> se::l) [] s

let printPG (l : Set<(Node*Action*Node)>) = printf "digraph PG {rankdir=LR; \nnode [shape = doublecircle]; q◀; \nnode [shape = circle]; q▷ ; \n%s}" (edge2string (toList(l))) |> ignore  
