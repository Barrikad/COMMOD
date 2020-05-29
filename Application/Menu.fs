module Menu

open System
open PrintAST
open ASTConstructor
open ProgramGraph
open Initializer
open Interpreter
open InitSigns
open PGSigns
open InitSecurity
open SecurityTransitions
open Security
open PrintSec
open PrintSA
open PrintPG
open PrintExecution

let private welcome () = 
    printfn "Welcome to COMMOD!"
    printfn "This is a program for analysing programs written in a guarded command language."
    printfn "-------------------------------------------------------------------------------"
    printfn "This analysis will be based on the files in the Input folder."
    printfn "" 
    printfn "Type \"d\" for deterministic analysis and"
    printfn "\"n\" for non-deterministic analysis."
    Console.ReadLine()

let private chooseAnalysis () = 
    printfn "Choose an action to perform on the GCP from the menu:"
    printfn "1. Construct an AST"
    printfn "2. Construct a program graph"
    printfn "3. Show end configurations"
    printfn "4. Trace step-wise execution"
    printfn "5. Perform sign analysis"
    printfn "6. Perform security analysis"
    printfn "7. Exit program"
    Console.ReadLine()

let private astChoice ast =
    printfn "\nSyntax explanation:"
    printfn "[Command],{Guarded Command},(Arithmetic Expression),<Boolean Expression>"
    printfn "Errors in the program will be displayed in the AST"
    printfn ""
    printfn "AST:"
    printCom ast

let private pgChoice pg =
    printfn "\nThe program graph of the GCP in graphviz format:"
    printPG pg

let private interpretChoice pg =
    printfn "\nFor this analysis to work correctly, initial memory has to be defined in the file \"initialMemory.txt\""
    printfn "Configuration(s) at end node:"
    printEnd (endConfigurations pg initialMemory)

let private stepWiseChoice pg = 
    printfn "\nFor this analysis to work correctly, initial memory has to be defined in the file \"initialMemory.txt\"."
    printfn "This process will always assume deterministic pg, for clarety"
    printfn "Steps of execution:"
    printExec (traversePG pg initialMemory)

let private signChoice pg =
    printfn "\nFor this analysis to work correctly, initial signs has to be defined in the file \"initialSigns.txt\"."
    printfn "Possible sign configurations:"
    printSA (signsInPG getInit pg)

let private secChoice pg =
    let actualFlows = flowsInPG pg
    let vios = violations actualFlows
    printfn "\nFor this analysis to work correctly, lattices and classifications has to be defined in the corresponding files."
    printfn "Allowed flows:"
    printFlows allowedFlows
    printfn "--------------"
    printfn "\nActual flows:"
    printFlows actualFlows
    printfn "--------------"
    printfn "\nViolations:"
    printViolations vios
    printfn "--------------"
    
    if Set.isEmpty vios then
        printfn "\nThe program is secure"
    else
        printfn "\nThe program has security violations"

    printfn "\n"


let rec startDialogue () = 
    let determinism = welcome ()
    let mutable analysis = "0"
    let AST = parse program
    let mutable PG = Set.empty
    let mutable stop = false

    try
        PG <- AST2PG AST (determinism = "d")
    with
    | ErrorInTreeException _ -> printf "There are errors in the AST. Other choices than 1. will not work correctly"

    while not stop do
        analysis <- chooseAnalysis ()
        if analysis = "1" then
            astChoice AST
        elif analysis = "2" then
            pgChoice PG
        elif analysis = "3" then
            interpretChoice PG
        elif analysis = "4" then
            stepWiseChoice PG
        elif analysis = "5" then
            signChoice PG
        elif analysis = "6" then
            secChoice PG
        elif analysis = "7" then
            stop <- true
        printfn "\n\n"
        printfn "-----------------------------------"