module SignAnalyser

open Types
open OperatorSigns

//let PG2Signs initialSignSet pg = 

let unionInsert abstractMemory key signSet = 
    match Map.tryFind key abstractMemory with
    | None -> Map.add key signSet abstractMemory
    | Some signSetX -> Map.add key (Set.union signSet signSetX) abstractMemory

let unionMemory abstractMemory1 abstractMemory2 = Map.fold unionInsert abstractMemory1 abstractMemory2

let signMap sign signSet operatorSign = Set.fold (fun signSetX signX -> Set.union (operatorSign sign signX) signSetX) Set.empty signSet

let powerSetSign signSet1 signSet2 operatorSign = Set.fold (fun signSetX signX -> Set.union signSetX (signMap signX signSet2 operatorSign)) Set.empty signSet1

let rec artmSign abstractMemory artm =
    match artm with
    | Int x          -> if x > 0 then Set.empty.Add Positive else if x < 0 then Set.empty.Add Negative else Set.empty.Add Zero
    | Var str         -> match (Map.find str abstractMemory) with
                         | VarSign s -> Set.empty.Add s 
                         | _ -> raise (ErrorInAbsMemException "Variable interpreted as array")
    | Arr(str, exp) -> match (Map.find str abstractMemory) with
                          | ArrSign s -> if Set.isEmpty (Set.intersect (artmSign abstractMemory exp) ((Set.empty.Add Positive).Add Zero)) then Set.empty else s
                          | _ -> raise (ErrorInAbsMemException "Array interpreted as variable")
    | Plus (x1, x2)  -> powerSetSign (artmSign abstractMemory x1) (artmSign abstractMemory x2) plusSign
    | Minus (x1, x2) -> powerSetSign (artmSign abstractMemory x1) (artmSign abstractMemory x2) minusSign
    | Times (x1, x2) -> powerSetSign (artmSign abstractMemory x1) (artmSign abstractMemory x2) timesSign
    | Div (x1, x2)   -> powerSetSign (artmSign abstractMemory x1) (artmSign abstractMemory x2) divSign
    | Neg x          -> unaryMinusSign (artmSign abstractMemory x)
    | Pow (x1, x2)   -> powerSetSign (artmSign abstractMemory x1) (artmSign abstractMemory x2) powSign
    | AError _ -> raise (ErrorInPGException "Can not interpret erroneous PG")

let rec booleanSign abstractMemory boolean =
    match boolean with
    | True ->  Set.empty.Add true 
    | False ->  Set.empty.Add false
    | And (a, b) -> powerSetSign (booleanSign abstractMemory a) (booleanSign abstractMemory b) (primitiveBoolSign (&&))
    | Or (a, b) -> powerSetSign (booleanSign abstractMemory a) (booleanSign abstractMemory b) (primitiveBoolSign (||))
    | AndSC (a, b) -> powerSetSign (booleanSign abstractMemory a) (booleanSign abstractMemory b) (primitiveBoolSign (&&))
    | OrSC (a, b) -> powerSetSign (booleanSign abstractMemory a) (booleanSign abstractMemory b) (primitiveBoolSign (||))
    | Not (a) -> Set.map (not) (booleanSign abstractMemory a)
    | Equal (a, b) -> powerSetSign (artmSign abstractMemory a) (artmSign abstractMemory b) equalSign
    | Greater (a, b) -> powerSetSign (artmSign abstractMemory a) (artmSign abstractMemory b) greaterSign
    | Lesser (a, b) -> powerSetSign (artmSign abstractMemory a) (artmSign abstractMemory b) lesserSign
    | BError _ -> raise (ErrorInPGException("Can not interpret erroneous PG"))

let addSign str sign abstractMemory = 
    match Map.tryFind str abstractMemory with
    | None -> Map.add str (ArrSign (Set.empty.Add sign)) abstractMemory
    | Some (ArrSign signSet) -> Map.add str (ArrSign (Set.add sign signSet)) abstractMemory
    |  _ -> raise (ErrorInAbsMemException "Array interpreted as variable")

let rec gcomSign abstractMemory gcom =
    match gcom with
    | Gives(bool,com) -> let boolSet = (booleanSign abstractMemory bool)
                         let executed = if Set.contains true boolSet then comSign abstractMemory com else Set.empty
                         if Set.contains false boolSet then Set.add abstractMemory executed else executed
    | Else(gcom1,gcom2) -> Set.union (gcomSign abstractMemory gcom1) (gcomSign abstractMemory gcom2)
    | GError _ -> raise (ErrorInPGException("Can not interpret erroneous PG"))

and comSign abstractMemory com =
    match com with
    | Assign(str,artm) -> Set.fold (fun memorySet sign -> Set.add (Map.add str (VarSign sign) abstractMemory) memorySet) Set.empty (artmSign abstractMemory artm)
    | ArrAssign(str,artm1,artm2) -> if Set.isEmpty (Set.intersect (artmSign abstractMemory artm1) ((Set.empty.Add Positive).Add Zero)) then Set.empty 
                                    else Set.fold (fun memorySet sign -> Set.add (addSign str sign abstractMemory) memorySet) Set.empty (artmSign abstractMemory artm2)
    | Skip -> Set.empty.Add abstractMemory
    | Coms(com1,com2) -> Set.fold (fun memorySet absMem -> Set.union (comSign absMem com2) memorySet) Set.empty (comSign abstractMemory com1)
    | If(gcom) -> gcomSign abstractMemory gcom
    | Do(gcom) -> gcomSign abstractMemory gcom
    | CError _ -> raise (ErrorInPGException("Can not interpret erroneous PG"))