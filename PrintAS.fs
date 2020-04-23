module PrintSA

type Node = 
    | StartNode
    | EndNode
    | MidNode of int

type Sign =
| Positive
| Negative
| Zero

type SignAssignment =
    | VarSign of string * Sign
    | ArrSign of string * Set<Sign>

let rec orderOfSigns x =
    match x with
    | VarSign (a, b)::tail -> [a] @ orderOfSigns tail
    | ArrSign (a, b)::tail -> [a] @ orderOfSigns tail
    | [] -> []
let toList s = Set.fold (fun l se -> se::l) [] s
let sign2string = function
    | Positive -> "+"
    | Negative -> "-"
    | Zero -> "0"
let rec signlist2string = function
    | x::tail -> sign2string x + "," + signlist2string tail
    | [] -> ""

let rec signMatching ss oo =
    match ss with
    | VarSign (a,b)::tail2 when a=oo -> sign2string b + " "
    | ArrSign (a,b)::tail2 when a=oo -> "{" + signlist2string (toList b) + "} "
    | VarSign (_)::tail2 -> signMatching tail2 oo
    | ArrSign (_)::tail2 -> signMatching tail2 oo
    | [] -> failwith "No sign associated with var"


let rec putInOrder (ss : SignAssignment list) (o : string list) =
    match o with
    | x::tail -> signMatching ss x + putInOrder ss tail           
    | [] -> ""


let rec printSA (s : Set<SignAssignment> list) (o : string list) =
    match s with
    | x::tail -> putInOrder (toList x) o + "\n" + printSA tail o
    | [] -> ""
let rec st2s = function
| x::tail -> x + " " + st2s tail 
| [] -> ""

let printSignAnalysis (s : Set<SignAssignment> list) = printf "%s" (st2s (orderOfSigns(toList(s.Head))) + (printSA s (orderOfSigns(toList(s.Head))))) |> ignore
