module Helpers.GrammarHelpers

open Core.Term
open Core.Operators

/// Creates a new term from a string (used in the parser)
let ListTermToTerm (l:Term list) : Term =
    let mutable res = EmptyList
    let l = List.rev l
    for i in l do
        res <- ConsList(i, res)
    res
    
let LetArgsToTerm (name: string) (args: string list) (b: Term) (c: Term) : Term =
    let mutable res = b
    for arg in args |> List.rev do
        res <- Abs(arg, res)
        
    Let(name, res, c)
    

let LetArgsToTermFix (name: string) (args: string list) (b: Term) (c: Term) : Term =
    let mutable res = b
    for arg in args |> List.rev do
        res <- Abs(arg, res)
        
    Let(name, Fix(name, res), c)
    
let FunToTerm (args: string list) (b: Term) : Term =
    let mutable res = b
    for arg in args |> List.rev do
        res <- Abs(arg, res)
        
    res