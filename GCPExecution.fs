module GCPExecution

open ASTType
open GCPProgramGraph

exception ErrorInPGException of string      

let rec execArtm mem arithm =
    match arithm with
    | Int x          -> x
    | Float x        -> (int x)
    | Val str         -> Map.find str (fst mem)
    | ArrVal(str, exp) -> List.item (execArtm mem exp) (Map.find str (snd mem))

    | Plus (x1, x2)  -> (execArtm mem x1) + (execArtm mem x2)
    | Minus (x1, x2) -> (execArtm mem x1) - (execArtm mem x2)
    | Times (x1, x2) -> (execArtm mem x1) * (execArtm mem x2)
    | Div (x1, x2)   -> (execArtm mem x1) / (execArtm mem x2)
    | Neg x          -> - (execArtm mem x)
                            
    | ParA x         -> execArtm mem x
    | Pow (x1, x2)   -> (int(float(execArtm mem x1) ** float(execArtm mem x2)))
                             
    | AError _ -> raise (ErrorInPGException("Can not interpret erroneous PG"))


let rec execBool mem boolean =
    match boolean with
    | True ->  true 
    | False ->  false
    //The difference between short circuited and non-short circuited operators are nonexistant, because we don't use side effects.
    //We still include the proper evaluations, in case we change the definitions of other operators.
    | And (a, b) -> let evaluatedA = execBool mem a 
                    let evaluatedB = execBool mem b
                    evaluatedA && evaluatedB
    | Or (a, b) -> let evaluatedA = execBool mem a
                   let evaluatedB = execBool mem b
                   evaluatedA || evaluatedB
    | AndSC (a, b) -> (execBool mem a) && (execBool mem b)
    | OrSC (a, b) -> (execBool mem a) || (execBool mem b)
    | Not (a) -> not (execBool mem a)
    | Equal (a, b) -> (execArtm mem a) = (execArtm mem b)
    | Greater (a, b) -> (execArtm mem a) > (execArtm mem b)
    | Lesser (a, b) -> (execArtm mem a) < (execArtm mem b)
    | ParB (a) -> execBool mem a
    | BError _ -> raise (ErrorInPGException("Can not interpret erroneous PG"))

let rec execCom mem com =

    let rec replaceItem list index newElement curIndex = 
        match list with
        | _::xs when curIndex = index -> newElement :: xs
        | x::xs -> x :: (replaceItem xs index newElement (curIndex + 1))
        | [] -> failwith "Array index not found"

    match com with
    | Assign(str,artm) -> let memWithoutStr = Map.remove str (fst mem)
                          (Map.add str (execArtm mem artm) memWithoutStr, snd mem)
    | ArrAssign(str,artm1,artm2) -> let index = execArtm mem artm1
                                    let editedMemList = replaceItem (Map.find str (snd mem)) index (execArtm mem artm2) 0
                                    let memWithoutStr = Map.remove str (snd mem)
                                    (fst mem, Map.add str editedMemList memWithoutStr)
    | Skip -> mem
    | Coms(com1,com2) -> execCom (execCom mem com1) com2
    | If(gcom) -> execGcom mem gcom
    | Do(gcom) -> if execBool mem (doneGCommand gcom) then mem else execCom (execGcom mem gcom) com
    | CError _ -> raise (ErrorInPGException("Can not interpret erroneous PG"))

and execGcom mem gcom =
    match gcom with
    | Gives(bool,com) -> if execBool mem bool then execCom mem com else mem
    | Else(gcom1,gcom2) -> if execBool mem (doneGCommand gcom1) then execGcom mem gcom2 else execGcom mem gcom1
    | GError _ -> raise (ErrorInPGException("Can not interpret erroneous PG"))
