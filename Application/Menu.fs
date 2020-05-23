module Menu

open System
open PrintAST
open ASTConstructor
open ProgramGraph
open Initializer
open Interpreter
open PrintPG
open PrintExecution

let private welcome = 
    printfn "Welcome to COMMOD!"
    printfn "This is a program for analysing programs written in a guarded command language."
    printfn "-------------------------------------------------------------------------------"
    printfn "This analysis will be based on the files in the Input folder."
    printfn "" 
    printfn "Type \"d\" for deterministic analysis and"
    printfn "\"n\" for non-deterministic analysis."
    Console.ReadLine()

let private chooseAnalysis = 
    printfn "Choose an action to perform on the GCP from the menu:"
    printfn "1. Construct an AST"
    printfn "2. Construct a program graph"
    printfn "3. Show end configurations"
    printfn "4. Trace step-wise execution"
    printfn "5. Perform sign analysis"
    printfn "6. Perform security analysis"
    printfn "7. Perform a model checking"
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

let startDialogue = 
    let determinism = welcome
    let analysis = chooseAnalysis
    let AST = parse program

    if analysis = "1" then
        astChoice AST
    elif analysis = "2" then
        let PG = AST2PG AST (determinism = "d")
        pgChoice PG
    elif analysis = "3" then
        let PG = AST2PG AST (determinism = "d")
        interpretChoice PG
    elif analysis = "4" then
        let PG = AST2PG AST (determinism = "d")
        stepWiseChoice PG