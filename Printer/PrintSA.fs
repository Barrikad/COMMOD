module PrintSA

open Types

let rec orderOfSigns x =
    match x with
    | (a, _)::tail -> [a] @ orderOfSigns tail
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
    | (a,VarSign b)::tail2 when a=oo -> sign2string b + " "
    | (a,ArrSign b)::tail2 when a=oo -> "{" + signlist2string (toList b) + "} "
    | (_)::tail2 -> signMatching tail2 oo
    | [] -> failwith "No sign associated with var"


let rec putInOrder ss (o : string list) =
    match o with
    | x::tail -> signMatching ss x + putInOrder ss tail           
    | [] -> ""


let rec printSA (s : Set<string*SignAssignment> list) (o : string list) =
    match s with
    | x::tail -> putInOrder (toList x) o + "\n" + printSA tail o
    | [] -> ""
let rec st2s = function
| x::tail -> x + " " + st2s tail 
| [] -> ""

let printSignAnalysis (s : Set<string*SignAssignment> list) = printf "%s" (st2s (orderOfSigns(toList(s.Head))) + "\n" + (printSA s (orderOfSigns(toList(s.Head))))) |> ignore

