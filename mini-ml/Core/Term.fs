module Core.Term

open System
open Core.Operators

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
    | IfThen of Term * Term
    
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
    | IfThen (cond, t) -> $"if {string_of_term cond} then {string_of_term t}"
    
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
    | IfThen (cond, tr) -> IfThen(map_name cond f, map_name tr f)
    
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
    | InternalOperation op -> InternalOperation op
    | Bool b -> Bool b
    | IfThenElse (cond, tr, fs) -> IfThenElse(map_var cond f bo, map_var tr f bo, map_var fs f bo)
    | IfThen (cond, tr) -> IfThen(map_var cond f bo, map_var tr f bo)
    
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
    | InternalOperation op -> InternalOperation op
    | Bool b -> Bool b
    | IfThenElse (cond, tr, fs) -> IfThenElse(convert cond, convert tr, convert fs)
    | IfThen (cond, tr) -> IfThen(convert cond, convert tr)
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
            | Not ->
                match r with
                | Bool b -> Bool(not b), true
                | _ -> reduce r
            | Empty ->
                match r with
                | EmptyList -> Bool true, true
                | ConsList _ -> Bool false, true
                | _ -> reduce r
            | Zero ->
                match r with
                | Num 0 -> Bool true, true
                | Num x when x <> 0 -> Bool false, true
                | _ -> reduce r
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
    | IfThen (cond, tr) ->
        let newCond, has_reducedCond = reduce cond
        let newTr, has_reducedTr = reduce tr

        match newCond with
        | Bool true -> newTr, true
        | Bool false -> EmptyList, true
        | _ -> IfThen(newCond, newTr), has_reducedCond || has_reducedTr
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
