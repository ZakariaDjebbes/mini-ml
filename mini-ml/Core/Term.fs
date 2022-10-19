module Core.Term

open System
open Core.Operators.InternalOperator
open Core.Operators.BinaryOperator

type Term =
    | Var of string
    | Num of int
    | App of Term * Term
    | Abs of string * Term
    | BinaryOperation of Term * Term * BinaryOperator
    | ConsList of Term * Term
    | EmptyList
    | InternalOperation of InternalOperator
    | Bool of bool
    | IfThenElse of Term * Term * Term
    | Fix of string * Term
    
/// Get a readable string representation of a term
let rec string_of_term term =
    match term with
    | Var x -> x
    | Num n -> $"{n.ToString()}"
    | App (l, r) ->
        "("
        + string_of_term l
        + " "
        + string_of_term r
        + ")"
    | Abs (x, t) -> "(λ" + x + "." + string_of_term t + ")"
    | BinaryOperation (l, r, op) -> $"({string_of_term l} {string_of_binary_perator op} {string_of_term r})"
    | ConsList (l, r) -> $"({string_of_term l} :: {string_of_term r})"
    | EmptyList -> "[]"
    | InternalOperation op -> string_of_internal_operator op
    | Bool b -> $"{b.ToString().ToLower()}"
    | IfThenElse (cond, t, f) -> $"if {string_of_term cond} then {string_of_term t} else {string_of_term f}"
    | Fix (x, t) -> $"fix ({x}) = {string_of_term t}"
/// Pretty print a term
let pretty_print_term term = printfn $"%s{string_of_term term}"

/// Map a variable name to a new variable name using a renaming function
let rec map_name term f =
    match term with
    | Var x -> Var(f x)
    | Num n -> Num n
    | App (l, r) -> App(map_name l f, map_name r f)
    | Abs (x, t) -> Abs(f x, map_name t f)
    | BinaryOperation (l, r, op) -> BinaryOperation(map_name l f, map_name r f, op)
    | ConsList (l, r) -> ConsList(map_name l f, map_name r f)
    | EmptyList -> EmptyList
    | InternalOperation op -> InternalOperation op
    | Bool b -> Bool b
    | IfThenElse (cond, tr, fs) -> IfThenElse(map_name cond f, map_name tr f, map_name fs f)
    | Fix (x, t) -> Fix(f x, map_name t f)
/// Rename a variable name to a new variable name
let rec map_var term func bo =
    match term with
    | Var x -> func x bo
    | Num n -> Num n
    | App (l, r) -> App(map_var l func bo, map_var r func bo)
    | Abs (x, t) -> Abs(x, map_var t func (x :: bo))
    | BinaryOperation (l, r, op) -> BinaryOperation(map_var l func bo, map_var r func bo, op)
    | ConsList(l, r) -> ConsList(map_var l func bo, map_var r func bo)
    | EmptyList -> EmptyList
    | InternalOperation op -> InternalOperation op
    | Bool b -> Bool b
    | IfThenElse (cond, tr, fs) -> IfThenElse(map_var cond func bo, map_var tr func bo, map_var fs func bo)
    | Fix (x, t) -> Fix(x, map_var t func (x :: bo))
/// Substitute a name for a variable in a term
let substitute_name term from changeTo =
    map_name term (fun x -> if x = from then changeTo else x)

/// Substitute a term for a variable in a term
let substitute_var term from changeTo =
    map_var
        term
        (fun x bo ->
            if x = from && not (List.contains x bo) then
                changeTo
            else
                Var x)
        []

/// Get a fresh variable name
let name_factory =
    let counter = ref -1

    fun () ->
        counter.Value <- counter.Value + 1
        "$" + counter.Value.ToString()

/// Converts a term to a new fresh name
let rec convert term =
    match term with
    | Abs (x, t) ->
        let newName = name_factory ()
        Abs((if x.StartsWith('@') then newName else x),
             let subbed = substitute_name t x newName
             convert subbed)
    | App (l, r) -> App(convert l, convert r)
    | Var x -> Var x
    | Num n -> Num n
    | BinaryOperation (l, r, op) -> BinaryOperation(convert l, convert r, op)
    | ConsList(l, r) -> ConsList(convert l, convert r)
    | EmptyList -> EmptyList
    | InternalOperation op -> InternalOperation op
    | Bool b -> Bool b
    | IfThenElse (cond, tr, fs) -> IfThenElse(convert cond, convert tr, convert fs)
    | Fix (x, t) -> Fix(x, convert t)
/// Alpha convert a term
let alpha_convert term =
    let converted = convert (map_name term (fun x -> "@" + x))
    map_var converted (fun x _ -> (
        let name = x[1..] // remove first char (the @)
        let internalOperator = internal_operator_of_string name
        
        match internalOperator with
        | Some op -> InternalOperation op
        | None -> Var x
    )) []
/// Do a single reduction step
let rec reduce term =
    match term with
    | App (l, r) ->
        let rm, has_reduced_m = reduce l
        let rn, has_reduced_n = reduce r
        match rm with
        | Abs (x, t) -> substitute_var t x rn, true
        | InternalOperation op ->
            match op, r with
            | Head, ConsList (elem, _) -> elem, true
            | Head, EmptyList -> raise(NotSupportedException("Trying to get the head of an empty list"))
            | Tail, ConsList(_, list) -> list, true
            | Tail, EmptyList -> raise(NotSupportedException("Trying to get tail of non-list"))
            | Not, Bool b -> Bool(not b), true
            | Empty, EmptyList -> Bool true, true
            | Empty, ConsList _ -> Bool false, true
            | _, _ -> App(rm, rn), has_reduced_m || has_reduced_n
        | _ -> App(rm, rn), has_reduced_m || has_reduced_n
    | Abs (x, t) -> Abs(x, t), false
    | BinaryOperation (l, r, op) ->
        let newL, has_reduced_l = reduce l
        let newR, has_reduced_r = reduce r

        match newL, newR with
        | Num n1, Num n2 ->
            match op with
            | Plus -> Num(n1 + n2), true
            | Minus -> Num(n1 - n2), true
            | Times -> Num(n1 * n2), true
            | Divide -> Num(n1 / n2), true
            | Mod -> Num(n1 % n2), true
            | Equals -> Bool(n1 = n2), true
            | NotEquals -> Bool(n1 <> n2), true
            | LessThan -> Bool(n1 < n2), true
            | LessThanOrEqual -> Bool(n1 <= n2), true
            | GreaterThan -> Bool(n1 > n2), true
            | GreaterThanOrEqual -> Bool(n1 >= n2), true
            | _ -> raise(NotSupportedException($"Invalid usage of binary operator {string_of_binary_perator op} on non-numbers"))
        | Bool b1, Bool b2 ->
            match op with
            | And -> Bool(b1 && b2), true
            | Or -> Bool(b1 || b2), true
            | Equals -> Bool(b1 = b2), true
            | NotEquals -> Bool(b1 <> b2), true
            | _ -> raise(NotSupportedException($"Invalid usage of binary operator {string_of_binary_perator op} on non-booleans"))
        | _ -> BinaryOperation(newL, newR, op), has_reduced_l || has_reduced_r
    | ConsList (l, r) ->
        let newL, has_reducedL = reduce l
        let newR, has_reducedR = reduce r
        
        ConsList(newL, newR), (has_reducedL || has_reducedR)
    | InternalOperation op -> InternalOperation op, false
    | IfThenElse (cond, tr, fs) ->
        let newCond, has_reducedCond = reduce cond
        let newTr, has_reducedTr = reduce tr
        let newFs, has_reducedFs = reduce fs

        match newCond with
        | Bool true -> newTr, true
        | Bool false -> newFs, true
        | _ -> IfThenElse(newCond, newTr, newFs), has_reducedCond || has_reducedTr || has_reducedFs
    | Fix (x, t) ->
        let newT = substitute_var t x (Fix(x, t))
        printfn $"> > > %s{string_of_term newT}"
        
        newT, true
    | Var _ | Num _ | EmptyList | Bool _ -> term, false
/// Fully evaluate a term, throwing an exception if it takes too long
let evaluate term =
    let mutable redT: Term = term
    let mutable should_reduce = true

    let stopWatch =
        System.Diagnostics.Stopwatch.StartNew()

    while should_reduce do
        if stopWatch.Elapsed.Seconds = 5 then
            raise (TimeoutException("Timed out after 5 seconds, looping evaluation."))

        let reduced, has_reduced = reduce redT
        redT <- reduced
        should_reduce <- has_reduced

    redT
