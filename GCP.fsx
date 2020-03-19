#r "C:/Users/simon/FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"

open FSharp.Text.Lexing
open System

#load "TypeAST.fs"
open TypeAST

#load "PrintAST.fs"
open PrintAST

#load "GCPParser.fs"
open GCPParser

#load "GCPLexer.fs"
open GCPLexer

#load "progGraph.fs"
open progGraph

let program = IO.File.ReadAllText "GCProgram.txt"

let parse input =
    let lexbuf = LexBuffer<char>.FromString input
    let res = start tokenize lexbuf
    res


printCom (parse program)
printfn " "