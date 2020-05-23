module OperatorSigns

open Types

let plusSign sign1 sign2 = 
    match sign1 with
    | Positive -> match sign2 with
                  | Positive -> Set.empty.Add Positive
                  | Zero -> Set.empty.Add Positive
                  | Negative -> Set.add Negative (Set.add Zero (Set.empty.Add Positive))
    | Zero -> match sign2 with
                  | Positive -> Set.empty.Add Positive
                  | Zero -> Set.empty.Add Zero
                  | Negative -> Set.empty.Add Negative
    | Negative -> match sign2 with
                  | Negative -> Set.empty.Add Negative
                  | Zero -> Set.empty.Add Negative
                  | Positive -> Set.add Negative (Set.add Zero (Set.empty.Add Positive))

let minusSign sign1 sign2 = 
    match sign1 with
    | Positive -> match sign2 with
                  | Positive -> Set.add Negative (Set.add Zero (Set.empty.Add Positive))
                  | Zero -> Set.empty.Add Positive
                  | Negative -> Set.empty.Add Positive
    | Zero -> match sign2 with
                  | Positive -> Set.empty.Add Negative
                  | Zero -> Set.empty.Add Zero
                  | Negative -> Set.empty.Add Positive
    | Negative -> match sign2 with
                  | Negative -> Set.add Negative (Set.add Zero (Set.empty.Add Positive))
                  | Zero -> Set.empty.Add Negative
                  | Positive -> Set.empty.Add Negative

let timesSign sign1 sign2 = 
    match sign1 with
    | Positive -> match sign2 with
                  | Positive -> Set.empty.Add Positive
                  | Zero -> Set.empty.Add Zero
                  | Negative -> Set.empty.Add Negative
    | Zero -> match sign2 with
                  | Positive -> Set.empty.Add Zero
                  | Zero -> Set.empty.Add Zero
                  | Negative -> Set.empty.Add Zero
    | Negative -> match sign2 with
                  | Negative -> Set.empty.Add Positive
                  | Zero -> Set.empty.Add Zero
                  | Positive -> Set.empty.Add Negative

let divSign sign1 sign2 =
    match sign1 with
    | Positive -> match sign2 with
                  | Positive -> Set.empty.Add Positive
                  | Zero -> Set.empty
                  | Negative -> Set.empty.Add Negative
    | Zero -> match sign2 with
                  | Positive -> Set.empty.Add Zero
                  | Zero -> Set.empty
                  | Negative -> Set.empty.Add Zero
    | Negative -> match sign2 with
                  | Negative -> Set.empty.Add Positive
                  | Zero -> Set.empty
                  | Positive -> Set.empty.Add Negative

let powSign sign1 sign2 = 
    match sign1 with
    | Positive -> Set.empty.Add Positive
    | Zero -> match sign2 with
                  | Positive -> Set.empty.Add Zero
                  | Zero -> Set.empty
                  | Negative -> Set.empty.Add Zero
    | Negative -> match sign2 with
                  | Negative -> (Set.empty.Add Negative).Add Positive
                  | Zero -> Set.empty.Add Positive
                  | Positive -> (Set.empty.Add Negative).Add Positive

let unaryMinusSignAtomic sign = 
    match sign with
    | Positive -> Negative
    | Zero -> Zero
    | Negative -> Positive

let unaryMinusSign signSet = Set.map unaryMinusSignAtomic signSet

let primitiveBoolSign boolSign boolean1 boolean2 = Set.empty.Add (boolSign boolean1 boolean2)

let equalSign sign1 sign2 = 
    match sign1 with
    | Positive -> match sign2 with
                  | Positive -> (Set.empty.Add true).Add false
                  | Zero -> Set.empty.Add false
                  | Negative -> Set.empty.Add false
    | Zero -> match sign2 with
                  | Positive -> Set.empty.Add false
                  | Zero -> Set.empty.Add true
                  | Negative -> Set.empty.Add false
    | Negative -> match sign2 with
                  | Negative -> Set.empty.Add false
                  | Zero -> Set.empty.Add false
                  | Positive -> (Set.empty.Add true).Add false

let greaterSign sign1 sign2 = 
    match sign1 with
    | Positive -> match sign2 with
                  | Positive -> (Set.empty.Add true).Add false
                  | Zero -> Set.empty.Add true
                  | Negative -> Set.empty.Add true
    | Zero -> match sign2 with
                  | Positive -> Set.empty.Add false
                  | Zero -> Set.empty.Add false
                  | Negative -> Set.empty.Add true
    | Negative -> match sign2 with
                  | Negative -> (Set.empty.Add true).Add false
                  | Zero -> Set.empty.Add false
                  | Positive -> Set.empty.Add false

let lesserSign sign1 sign2 = 
    match sign1 with
    | Positive -> match sign2 with
                  | Positive -> (Set.empty.Add true).Add false
                  | Zero -> Set.empty.Add false
                  | Negative -> Set.empty.Add false
    | Zero -> match sign2 with
                  | Positive -> Set.empty.Add true
                  | Zero -> Set.empty.Add false
                  | Negative -> Set.empty.Add false
    | Negative -> match sign2 with
                  | Negative -> (Set.empty.Add true).Add false
                  | Zero -> Set.empty.Add true
                  | Positive -> Set.empty.Add true

