module ASTConstructor

open System
open FSharp.Text.Lexing
open Types
open Lexer
open Parser

let program = IO.File.ReadAllText "Input\\GCProgram.txt"

let parse input =
    let lexbuf = LexBuffer<char>.FromString input
    let res = start tokenize lexbuf
    res