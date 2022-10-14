module Core.Term

open System

type Operator =
    | Plus
    | Minus
    | Times
    | Div
    | Mod

type Term =
    | Var of string
    | Num of int
    | App of Term * Term
    | Abs of string * Term
    | BinaryOperation of Term * Term * Operator
    | ConsList of Term * Term
    | EmptyList 
    | Let of string * Term * Term

let ListTermToTerm (l:Term list) : Term =
    let mutable res = EmptyList
    let l = List.rev l
    for i in l do
        res <- ConsList(i, res)
    res

/// Gets the string of an operator to be used in the pretty printer.
let string_of_operator op =
    match op with
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Div -> "/"
    | Mod -> "%"

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
    | BinaryOperation (l, r, op) -> $"({string_of_term l} {string_of_operator op} {string_of_term r})"
    | ConsList (l, r) -> $"({string_of_term l} :: {string_of_term r})"
    | EmptyList -> "[]"
    | Let (x, e1, e2) -> $"(let {x} = {string_of_term e1} in {string_of_term e2})"
    
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
    | Let (x, e1, e2) -> Let(f x, map_name e1 f, map_name e2 f)
    
/// Rename a variable name to a new variable name
let rec map_var t f bo =
    match t with
    | Var x -> f x bo
    | Num n -> Num n
    | App (l, r) -> App(map_var l f bo, map_var r f bo)
    | Abs (x, t) -> Abs(x, map_var t f (x :: bo))
    | BinaryOperation (l, r, op) -> BinaryOperation(map_var l f bo, map_var r f bo, op)
    | ConsList(l, r) -> ConsList(map_var l f bo, map_var r f bo)
    | EmptyList -> EmptyList
    | Let (x, e1, e2) -> Let(x, map_var e1 f bo, map_var e2 f (x :: bo))
    
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
    | Var x -> Var x
    | Num n -> Num n
    | App (l, r) -> App(convert l, convert r)
    | Abs (x, t) ->
        let newName = name_factory ()

        Abs(
            (if x.StartsWith('@') then
                 newName
             else
                 x),
            substitute_name (convert t) x newName
        )
    | BinaryOperation (l, r, op) -> BinaryOperation(convert l, convert r, op)
    | ConsList(l, r) -> ConsList(convert l, convert r)
    | EmptyList -> EmptyList
    | Let (x, e1, e2) ->
        let newName = name_factory ()

        Let(
            (if x.StartsWith('@') then
                 newName
             else
                 x),
            convert e1,
            substitute_name (convert e2) x newName
        )
/// Alpha convert a term
let alpha_convert term =
    convert (map_name term (fun x -> "@" + x))

/// Do a single reduction step
let rec reduce term =
    match term with
    | App (l, r) ->
        let rm, has_reduced_m = reduce l
        let rn, has_reduced_n = reduce r

        match rm with
        | Abs (x, t) -> substitute_var t x rn, true
        | _ -> App(rm, rn), has_reduced_m || has_reduced_n
    | Abs (x, t) ->
        let t, has_reduced = reduce t
        Abs(x, t), has_reduced
    | BinaryOperation (l, r, op) ->
        let newL, has_reduced_l = reduce l
        let newR, has_reduced_r = reduce r

        match newL, newR with
        | Num n1, Num n2 ->
            Num(
                match op with
                | Plus -> n1 + n2
                | Minus -> n1 - n2
                | Times -> n1 * n2
                | Div -> n1 / n2
                | Mod -> n1 % n2
            ),
            true
        | _ -> BinaryOperation(newL, newR, op), has_reduced_l || has_reduced_r
    | ConsList (l, r) ->
        let newL, has_reducedL = reduce l
        let newR, has_reducedR = reduce r
        
        ConsList(newL, newR), (has_reducedL || has_reducedR)
    | Let (x, e1, e2) ->
        let newE1, has_reduced_e1 = reduce e1
        let newE2, has_reduced_e2 = reduce e2

        match newE1 with
        | Abs (y, t) -> substitute_var t y newE2, true
        | _ -> Let(x, newE1, newE2), has_reduced_e1 || has_reduced_e2
    | Var _ | Num _ | EmptyList -> term, false
    
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
