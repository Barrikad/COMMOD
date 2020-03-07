#r "C:/Users/simon/FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"

open FSharp.Text.Lexing
open System

#load "TypeAST.fs"

open TypeAST

#load "GCPParser.fs"

open GCPParser

#load "GCPLexer.fs"

open GCPLexer

let parse input =
    let lexbuf = LexBuffer<char>.FromString input
    let res = GCPParser.start GCPLexer.tokenize lexbuf
    res

let rec printCom (com : Command) = 
    printf " ["
    match com with
    | Assign(str,artm) -> printf "%s := " str
                          printArtm artm
    | ArrAssign(str,artm1,artm2) -> printf "%s[" str
                                    printArtm artm1
                                    printf "] := "
                                    printArtm artm2
    | Skip -> printf "Skip "
    | If(gcom) -> printf "if "
                  printGcom gcom
    | Do(gcom) -> printf "do "
                  printGcom gcom
    | Coms(com1,com2) -> printCom com1
                         printf "; "
                         printCom com2
    printf "] "
and printGcom (gcom : GCommand) =
    printf " {"
    match gcom with
    | Gives(bol, com) -> printBool bol
                         printf " -> "
                         printCom com
    | Else(gcom1,gcom2) -> printGcom gcom1
                           printf " [] "
                           printGcom gcom2
    printf "} "

and printArtm (artm : Arithm) = 
    printf " ("
    match artm with
    | Int(x) -> printf "%i " x
    | Float(x) -> printf "%f " x
    | Val(str) -> printf "%s " str
    | ArrVal(str,artm) -> printf "%s[" str
                          printArtm artm
                          printf "] "
    | Plus(artm1,artm2) -> printArtm artm1
                           printf " + "
                           printArtm artm2
    | Minus(artm1,artm2) -> printArtm artm1
                            printf " - "
                            printArtm artm2
    | Times(artm1,artm2) -> printArtm artm1
                            printf " * "
                            printArtm artm2
    | Div(artm1,artm2) -> printArtm artm1
                          printf " / "
                          printArtm artm2
    | Pow(artm1,artm2) -> printArtm artm1
                          printf " ^ "
                          printArtm artm2
    | Neg(artm) -> printf "-"
                   printArtm artm
    | ParA(artm) -> printf " ("
                    printArtm artm
                    printf ") "
    printf ") "
and printBool (bol : Boolean) = 
    printf " <"
    match bol with
    | True -> printf " True "
    | False -> printf " False "
    | AndSC(bol1,bol2) -> printBool bol1
                          printf " && "
                          printBool bol2
    | And(bol1,bol2) -> printBool bol1
                        printf " & "
                        printBool bol2
    | OrSC(bol1,bol2) -> printBool bol1
                         printf " || "
                         printBool bol2
    | Or(bol1,bol2) -> printBool bol1
                       printf " | "
                       printBool bol2
    | Not(bol) -> printf "!"
                  printBool bol
    | ParB(bol) -> printf "("
                   printBool bol
                   printf ") "
    | Equal(artm1,artm2) -> printArtm artm1
                            printf " = "
                            printArtm artm2
    | Greater(artm1,artm2) -> printArtm artm1
                              printf " > "
                              printArtm artm2
    | Lesser(artm1,artm2) -> printArtm artm1
                             printf " < "
                             printArtm artm2
    printf "> "

printCom (parse "a := 3; if a = 3 -> a := a - 1 fi; skip ")