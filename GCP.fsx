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

let parse input =
    let lexbuf = LexBuffer<char>.FromString input
    let res = GCPParser.start GCPLexer.tokenize lexbuf
    res


printCom (parse "a := 3; if a = 3 -> a := a - 1 fi; skip ")