module Interpreter

open TypeAST

let rec artmOpToArtmExp x1 x2 oper mem : int option = 
    let u = execArtm mem x1
    let v = execArtm mem x2
    if u.IsSome && v.IsSome then
        Some (oper u.Value v.Value)
    else 
        None

and execArtm mem arithm : int option =
    match arithm with
    | Int x          -> Some x
    | Float x        -> Some (int x)
    | Val x          -> Map.tryFind x mem
    | ArrVal (v, exp)   -> let expr = execArtm mem exp
                           if expr.IsSome then
                                Map.tryFind (sprintf "%s[%u]" v expr.Value) mem
                           else
                            None
    | Plus (x1, x2)  -> artmOpToArtmExp x1 x2 ( + ) mem
    | Minus (x1, x2) -> artmOpToArtmExp x1 x2 ( - ) mem
    | Times (x1, x2) -> artmOpToArtmExp x1 x2 ( * ) mem
    | Div (x1, x2)   -> artmOpToArtmExp x1 x2 ( / ) mem
    | Neg x          -> let u = execArtm mem x
                        if u.IsSome then
                            Some (-(u.Value))
                        else 
                            None
    | ParA x         -> execArtm mem x
    | Pow (x1, x2)   -> let u = execArtm mem x1
                        let v = execArtm mem x2
                        if u.IsSome && v.IsSome && (v.Value >=0) then 
                            Some (int(float(u.Value) ** float(v.Value)))
                        else 
                            None
    | AError a -> None

let rec booleanOpToArtmExp x1 x2 oper mem : bool option =
    if (execArtm mem x1).IsSome && (execArtm mem x2).IsSome then
        Some (oper (execArtm mem x1).Value (execArtm mem x2).Value)
    else 
        None
let rec booleanOpToBoolExp x1 x2 oper mem : bool option =
    if (execBool mem x1).IsSome && (execBool mem x2).IsSome then
        Some (oper (execBool mem x1).Value (execBool mem x2).Value)
    else 
        None

and execBool mem boolean : bool option =
    match boolean with
    | True -> Some true 
    | False -> Some false
    | And (a, b) -> booleanOpToBoolExp a b ( && ) mem
    | Or (a, b) -> booleanOpToBoolExp a b ( || ) mem
    | AndSC (a, b) -> booleanOpToBoolExp a b ( && ) mem
    | OrSC (a, b) -> booleanOpToBoolExp a b ( || ) mem
    | Not (a) -> if execBool mem a = Some true then Some false else Some true
    | Equal (a, b) -> booleanOpToArtmExp a b ( = ) mem
    | Greater (a, b) -> booleanOpToArtmExp a b ( > ) mem
    | Lesser (a, b) -> booleanOpToArtmExp a b ( < ) mem
    | ParB (a) -> execBool mem a
    | BError a -> None


