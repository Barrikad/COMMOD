#load "FsLexYacc.10.0.0/src/fslex/Lexing.fs"
#load "FsLexYacc.10.0.0/src/fsyacc/Parsing.fs"
#load "Semantics/Types.fs"
#load "ProgramGraph/ProgramGraph.fs"
#load "Interpreter/Actions.fs"
#load "Interpreter/Interpreter.fs"
#load "Security/InitSecurity.fs"
#load "Security/SecurityTransitions.fs"
#load "Security/Security.fs"
#load "Printer/PrintSec.fs"
#load "Parser/Parser.fs"
#load "Parser/Lexer.fs"
#load "Parser/ASTConstructor.fs"
open Types
open InitSecurity
open SecurityTransitions
open Security
open PrintSec
open ProgramGraph
open ASTConstructor

let (ast:Command) = parse program

let pg = AST2PG ast true