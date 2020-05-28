module PrintSec

let printFlows flows = Map.fold (fun () key values -> Set.fold (fun () value -> printfn "%s -> %s" key value) () values) () flows
                        
let printViolations vios = Set.fold (fun () (key,value) -> printfn "%s -> %s" key value) () vios