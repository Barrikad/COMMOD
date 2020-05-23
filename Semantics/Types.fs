module Types

exception ErrorInPGException of string   
exception ErrorInAbsMemException of string

type Arithm =
    | Int of int
    | Var of string
    | Arr of (string*Arithm)
    | Plus of (Arithm*Arithm)
    | Minus of (Arithm*Arithm)
    | Times of (Arithm*Arithm)
    | Div of (Arithm*Arithm)
    | Neg of Arithm
    | Pow of (Arithm*Arithm)
    | AError of string

type Boolean =
    | True
    | False
    | And of (Boolean*Boolean)
    | Or of (Boolean*Boolean)
    | AndSC of (Boolean*Boolean)
    | OrSC of (Boolean*Boolean)
    | Not of Boolean
    | Equal of (Arithm*Arithm)
    | Greater of (Arithm*Arithm)
    | Lesser of (Arithm*Arithm)
    | BError of string

type GCommand =
    | Gives of (Boolean*Command)
    | Else of (GCommand*GCommand)
    | GError of string
and Command =
    | Assign of (string*Arithm)
    | ArrAssign of (string*Arithm*Arithm)
    | Skip
    | Coms of (Command*Command)
    | If of GCommand
    | Do of GCommand
    | CError of string

type Action =
    | BoolAct of Boolean
    | CommandAct of Command

type Node = 
    | StartNode
    | EndNode
    | MidNode of int

type Memory = Map<string, int> * Map<string,int list>

type State = 
    | Stuck      of Node * Memory
    | Terminated of Memory

type Sign =
    | Positive
    | Negative
    | Zero

type SignAssignment =
    | VarSign of Sign
    | ArrSign of Set<Sign>

