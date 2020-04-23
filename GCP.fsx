#r "C:/Users/simon/FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"

open FSharp.Text.Lexing
open System

#load "Types.fs"
open Types

#load "PrintAST.fs"
open PrintAST

#load "GCPParser.fs"
open GCPParser

#load "GCPLexer.fs"
open GCPLexer

#load "ProgramGraph.fs"
open ProgramGraph

#load "Execution.fs"
open Execution

#load "Interpreter.fs"
open Interpreter

#load "OperatorSigns.fs"
#load "SignAnalyser.fs"

#load "ProgramGraphSignAnalyser.fs"
open PGSigns

#load "PrintPG.fs"
open PrintPG

#load "PrintExecution.fs"
open PrintExecution

let program = IO.File.ReadAllText "GCProgram.txt"

let parse input =
    let lexbuf = LexBuffer<char>.FromString input
    let res = start tokenize lexbuf
    res


let pg = AST2PG (parse program) false
//printPG pg
printCom (parse program)
let signs = signsInPG (Map.ofList [("i",VarSign Positive);("x",VarSign Zero);("y",VarSign Zero);("A", ArrSign (((Set.empty.Add Zero).Add Negative).Add Positive))]) pg