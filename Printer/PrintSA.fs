module PrintSA

open Types

let private printSign sign = match sign with
                             | Positive -> printf "+"
                             | Negative -> printf "-"
                             | Zero -> printf "0"

let private printSet sign = printf " "
                            printSign sign

let private printVar var ass = printf "%s = " var
                               match ass with
                               | VarSign(s) -> printSign s
                               | ArrSign(sSet) -> Set.iter printSet sSet
                               printfn ""

let private printMap map = printfn "..........."
                           Map.iter printVar map
                           printfn "..........."

let private printAssignment assignment = printfn "-----------------------------------"
                                         Set.iter printMap assignment
                                         printfn "-----------------------------------"

let private printNodeSign node assignment = printf "\n\n\nq"
                                            match node with
                                            | StartNode -> printf "(start)"
                                            | EndNode -> printf "(end)"
                                            | MidNode(n) -> printf "(%i)" n
                                            printf ": "
                                            printfn ""
                                            printAssignment assignment

let printSA sa = Map.iter printNodeSign sa