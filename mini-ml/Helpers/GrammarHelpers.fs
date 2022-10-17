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