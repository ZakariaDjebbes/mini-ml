module Core.Term

open System
open Core.Operators

type Term =
    | Var of string
    | Num of int
    | App of Term * Term
    | Abs of string * Term
    | BinaryOperation of Term * Term * BinaryOperator
    | UnaryOperation of Term * UnaryOperator
    | ConsList of Term * Term
    | EmptyList
    | InternalOperation of InternalOperator
    | Bool of bool

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
    | UnaryOperation (t, op) -> $"({string_of_unary_operator op} {string_of_term t})"
    | ConsList (l, r) -> $"({string_of_term l} :: {string_of_term r})"
    | EmptyList -> "[]"
    | InternalOperation op -> string_of_internal_operator op
    | Bool b -> $"{b.ToString().ToLower()}"
    
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
    | UnaryOperation (t, op) -> UnaryOperation(map_name t f, op)
    | ConsList (l, r) -> ConsList(map_name l f, map_name r f)
    | EmptyList -> EmptyList
    | InternalOperation op -> InternalOperation op
    | Bool b -> Bool b
    
/// Rename a variable name to a new variable name
let rec map_var t f bo =
    match t with
    | Var x -> f x bo
    | Num n -> Num n
    | App (l, r) -> App(map_var l f bo, map_var r f bo)
    | Abs (x, t) -> Abs(x, map_var t f (x :: bo))
    | BinaryOperation (l, r, op) -> BinaryOperation(map_var l f bo, map_var r f bo, op)
    | UnaryOperation (t, op) -> UnaryOperation(map_var t f bo, op)
    | ConsList(l, r) -> ConsList(map_var l f bo, map_var r f bo)
    | EmptyList -> EmptyList
    | InternalOperation op -> InternalOperation op
    | Bool b -> Bool b
    
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
    | UnaryOperation (t, op) -> UnaryOperation(convert t, op)
    | ConsList(l, r) -> ConsList(convert l, convert r)
    | EmptyList -> EmptyList
    | InternalOperation op -> InternalOperation op
    | Bool b -> Bool b

/// Alpha convert a term
let alpha_convert term =
    convert (map_name term (fun x -> "@" + x))

/// Do a single reduction step
let rec reduce term =
    match term with
    | App (l, r) ->
        match l with
        | InternalOperation op ->
            match op with
            | Head ->
                match r with
                | ConsList (elem, _) -> elem, true
                | _ -> raise(NotSupportedException("Trying to get the head of a non-list"))
            | Tail ->
                match r with
                | ConsList (_, list) -> list, true
                | _ -> raise(NotSupportedException("Trying to get tail of non-list"))
            | Cons -> r, true // cher po
        | _ ->
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
                | Divide -> n1 / n2
                | Mod -> n1 % n2
                | _ -> raise(NotSupportedException($"Invalid usage of binary operator {string_of_binary_perator op} on non-numbers"))
            ),
            true
        | Bool b1, Bool b2 ->
            Bool(
                match op with
                | And -> b1 && b2
                | Or -> b1 || b2
                | _ -> raise(NotSupportedException($"Invalid usage of binary operator {string_of_binary_perator op} on non-booleans"))
            ),
            true
        | _ -> BinaryOperation(newL, newR, op), has_reduced_l || has_reduced_r
    | UnaryOperation (t, op) ->
        let newT, has_reduced = reduce t

        match newT with
        | Bool b ->
            Bool(
                match op with
                | Not -> not b
            ),
            true
        | _ -> UnaryOperation(newT, op), has_reduced
    | ConsList (l, r) ->
        let newL, has_reducedL = reduce l
        let newR, has_reducedR = reduce r
        
        ConsList(newL, newR), (has_reducedL || has_reducedR)
    | InternalOperation op -> InternalOperation op, false
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
