module Types

exception ErrorInPGException of string   
exception ErrorInAbsMemException of string

type Arithm =
    | Int of int
    | Val of string
    | ArrVal of (string*Arithm)
    | Plus of (Arithm*Arithm)
    | Minus of (Arithm*Arithm)
    | Times of (Arithm*Arithm)
    | Div of (Arithm*Arithm)
    | Neg of Arithm
    | Pow of (Arithm*Arithm)
    | ParA of Arithm
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
    | ParB of Boolean
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

//Further code for Interpreter
type Node = 
    | StartNode
    | EndNode
    | MidNode of int

type Memory = Map<string, int>

type State = 
    | Stuck      of Node * Memory
    | Terminated of Memory

//End Interpreter code

//Type for sign analysis

type Sign =
    | Positive
    | Negative
    | Zero

type SignAssignment =
    | VarSign of Sign
    | ArrSign of Set<Sign>

