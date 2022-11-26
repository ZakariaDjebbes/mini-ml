module Core.Term

open System
open Core.Operators.InternalOperator
open Core.Operators.BinaryOperators
open Core.NameFactory

type Term =
    | Var of string
    | Num of int
    | App of Term * Term
    | Abs of string * Term
    | NumOperation of Term * Term * NumOperator
    | BoolOperation of Term * Term * BoolOperator
    | ComparisonOperation of Term * Term * ComparisonOperator
    | ConsList of Term * Term
    | EmptyList
    | InternalOperation of InternalOperator
    | Bool of bool
    | Char of char
    | IfThenElse of Term * Term * Term
    | Fix of string * Term
    | Let of string * Term * Term
    | Pointer of int
    | Ref of Term
    | Deref of Term
    | Assign of Term * Term
    | Unit
    | Exception of MLException * Term
    | TryWith of Term * Term
    | Pair of Term * Term
    | Raise of MLException
and MLException =
    | BaseException
    | DivideByZero
    | HeadOfEmptyList
    | TailOfEmptyList
    | UnkownExceptionException
    
type Memory() =
    member val memory = Array.empty with get, set
    
    member this.at (index: int) =
        this.memory[index]
    
    member this.push (value: Term) =
        this.memory <- Array.append this.memory [|value|]
        this.memory.Length - 1
        
    member this.set (index: int) (value: Term) =
        this.memory[index] <- value
        
    member this.string_of_memory() =
        let mutable result = ""
        for i in 0..this.memory.Length - 1 do
            result <- result + $"%d{i}: %s{this.memory[i].ToString()}\n"
        result

let memory = Memory()
  
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
    | Abs _ -> "<function>"
    | NumOperation (l, r, op) -> $"({string_of_term l} {string_of_num_operator op} {string_of_term r})"
    | BoolOperation (l, r, op) -> $"({string_of_term l} {string_of_bool_operator op} {string_of_term r})"
    | ComparisonOperation (l, r, op) -> $"({string_of_term l} {string_of_comparison_operator op} {string_of_term r})"
    | ConsList (l, r) -> $"({string_of_term l} :: {string_of_term r})"
    | EmptyList -> "[]"
    | InternalOperation op -> string_of_internal_operator op
    | Bool b -> $"{b.ToString().ToLower()}"
    | Char c -> $"'{c.ToString()}'"
    | IfThenElse (cond, t, f) -> $"if {string_of_term cond} then {string_of_term t} else {string_of_term f}"
    | Fix (x, t) -> $"fix({x}, {string_of_term t})"
    | Let (x, t, b) -> $"let {x} = {string_of_term t} in {string_of_term b}"
    | Pointer n -> $"ptr({n.ToString()})"
    | Ref t -> $"ref({string_of_term t})"
    | Deref t -> $"!{string_of_term t}"
    | Assign (l, r) -> $"({string_of_term l} := {string_of_term r})"
    | Unit -> "<unit>"
    | Exception (e, t) -> $"<exception>: {string_of_exception e} caused by {string_of_term t}"
    | TryWith (t, c) -> $"try {string_of_term t} catch {string_of_term c}"
    | Pair (l, r) -> $"({string_of_term l}, {string_of_term r})"
    | Raise e -> $"raise {string_of_exception e}"
and string_of_term_debug term =
    match term with
    | Var x -> x
    | Num n -> $"{n.ToString()}"
    | App (l, r) ->
        "("
        + string_of_term_debug l
        + " "
        + string_of_term_debug r
        + ")"
    | Abs (x, t) -> "(λ" + x + "." + string_of_term_debug t + ")"
    | NumOperation (l, r, op) -> $"({string_of_term_debug l} {string_of_num_operator op} {string_of_term_debug r})"
    | BoolOperation (l, r, op) -> $"({string_of_term_debug l} {string_of_bool_operator op} {string_of_term_debug r})"
    | ComparisonOperation (l, r, op) -> $"({string_of_term_debug l} {string_of_comparison_operator op} {string_of_term_debug r})"
    | ConsList (l, r) -> $"({string_of_term_debug l} :: {string_of_term_debug r})"
    | EmptyList -> "[]"
    | InternalOperation op -> string_of_internal_operator op
    | Bool b -> $"{b.ToString().ToLower()}"
    | Char c -> $"'{c.ToString()}'"
    | IfThenElse (cond, t, f) -> $"if {string_of_term_debug cond} then {string_of_term_debug t} else {string_of_term_debug f}"
    | Fix (x, t) -> $"fix({x}, {string_of_term_debug t})"
    | Let (x, t, b) -> $"let {x} = {string_of_term_debug t} in {string_of_term_debug b}"
    | Pointer n -> $"ptr({n.ToString()})"
    | Ref t -> $"ref({string_of_term_debug t})"
    | Deref t -> $"!{string_of_term_debug t}"
    | Assign (l, r) -> $"({string_of_term_debug l} := {string_of_term_debug r})"
    | Unit -> "()"
    | Exception (e, t) -> $"<exception>: {string_of_exception e} caused by {string_of_term_debug t}"
    | TryWith (t, c) -> $"try {string_of_term_debug t} catch {string_of_term_debug c}"
    | Pair (l, r) -> $"({string_of_term_debug l}, {string_of_term_debug r})"
    | Raise e -> $"raise {string_of_exception e}"
 and string_of_exception e =
    match e with
    | BaseException -> "BaseException"
    | DivideByZero -> "DivdeByZero"
    | HeadOfEmptyList -> "HeadOfEmptyList"
    | TailOfEmptyList -> "TailOfEmptyList"
    | UnkownExceptionException -> "UnkownExceptionException"
/// Pretty print a term
let pretty_print_term term = printfn $"%s{string_of_term term}"

/// Map a variable name to a new variable name using a renaming function
let rec map_name term f =
    match term with
    | Var x -> Var(f x)
    | Num n -> Num n
    | App (l, r) -> App(map_name l f, map_name r f)
    | Abs (x, t) -> Abs(f x, map_name t f)
    | NumOperation (l, r, op) -> NumOperation(map_name l f, map_name r f, op)
    | BoolOperation (l, r, op) -> BoolOperation(map_name l f, map_name r f, op)
    | ComparisonOperation (l, r, op) -> ComparisonOperation(map_name l f, map_name r f, op)
    | ConsList (l, r) -> ConsList(map_name l f, map_name r f)
    | EmptyList -> EmptyList
    | InternalOperation op -> InternalOperation op
    | Bool b -> Bool b
    | IfThenElse (cond, tr, fs) -> IfThenElse(map_name cond f, map_name tr f, map_name fs f)
    | Fix (x, t) -> Fix(f x, map_name t f)
    | Let (x, t, b) -> Let(f x, map_name t f, map_name b f)
    | Pointer n -> Pointer n
    | Ref t -> Ref(map_name t f)
    | Deref t -> Deref(map_name t f)
    | Assign (l, r) -> Assign(map_name l f, map_name r f)
    | Unit -> Unit
    | Exception (e, t) -> Exception(e, map_name t f)
    | Char c -> Char c
    | TryWith (t, c) -> TryWith(map_name t f, map_name c f)
    | Pair (l, r) -> Pair(map_name l f, map_name r f)
    | Raise e -> Raise e
/// Rename a variable name to a new variable name
let rec map_var term func bo =
    match term with
    | Var x -> func x bo
    | Num n -> Num n
    | App (l, r) -> App(map_var l func bo, map_var r func bo)
    | Abs (x, t) -> Abs(x, map_var t func (x :: bo))
    | NumOperation (l, r, op) -> NumOperation(map_var l func bo, map_var r func bo, op)
    | BoolOperation (l, r, op) -> BoolOperation(map_var l func bo, map_var r func bo, op)
    | ComparisonOperation (l, r, op) -> ComparisonOperation(map_var l func bo, map_var r func bo, op)
    | ConsList(l, r) -> ConsList(map_var l func bo, map_var r func bo)
    | EmptyList -> EmptyList
    | InternalOperation op -> InternalOperation op
    | Bool b -> Bool b
    | IfThenElse (cond, tr, fs) -> IfThenElse(map_var cond func bo, map_var tr func bo, map_var fs func bo)
    | Fix (x, t) -> Fix(x, map_var t func (x :: bo))
    | Let (x, t, b) -> Let(x, map_var t func bo, map_var b func (x :: bo))
    | Pointer n -> Pointer n
    | Ref t -> Ref(map_var t func bo)
    | Deref t -> Deref(map_var t func bo)
    | Assign (l, r) -> Assign(map_var l func bo, map_var r func bo)
    | Unit -> Unit
    | Exception (e, t) -> Exception(e, map_var t func bo)
    | Char c -> Char c
    | TryWith (t, c) -> TryWith(map_var t func bo, map_var c func bo)
    | Pair (l, r) -> Pair(map_var l func bo, map_var r func bo)
    | Raise e -> Raise e
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
    | NumOperation (l, r, op) -> NumOperation(convert l, convert r, op)
    | BoolOperation (l, r, op) -> BoolOperation(convert l, convert r, op)
    | ComparisonOperation (l, r, op) -> ComparisonOperation(convert l, convert r, op)
    | ConsList(l, r) -> ConsList(convert l, convert r)
    | EmptyList -> EmptyList
    | InternalOperation op -> InternalOperation op
    | Bool b -> Bool b
    | IfThenElse (cond, tr, fs) -> IfThenElse(convert cond, convert tr, convert fs)
    | Fix (x, t) ->
        let newName = name_factory ()
        Fix((if x.StartsWith('@') then newName else x),
             let subbed = substitute_name t x newName
             convert subbed)
    | Let (a, b, c) ->
        let new_a = name_factory()
        let converted_b = convert b
        let subbed_c = substitute_name c a new_a
        let converted_c = convert subbed_c
        Let(new_a, converted_b, converted_c)
    | Pointer n -> Pointer n
    | Ref t -> Ref(convert t)
    | Deref t -> Deref(convert t)
    | Assign (l, r) -> Assign(convert l, convert r)
    | Unit -> Unit
    | Exception (e, t) -> Exception(e, convert t)
    | Char c -> Char c
    | TryWith (t, c) -> TryWith(convert t, convert c)
    | Pair (l, r) -> Pair(convert l, convert r)
    | Raise e -> Raise e
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
    
let rec is_non_expansive t =
    match t with
    | Var _ | Num _ | Bool _ | Abs _ | InternalOperation _ | EmptyList | Char _ | Raise _  -> true
    | NumOperation (l, r, _) | BoolOperation (l, r, _)
    | ComparisonOperation (l, r, _) | IfThenElse (_, l, r) | TryWith(l, r)
    | Let(_, l, r) | Pair(l, r) -> is_non_expansive l && is_non_expansive r    
    | Fix (_, t) -> is_non_expansive t
    | App _ | ConsList _ | Ref _ | Deref _ | Pointer _ | Assign _ -> false
    | Unit -> true
    | Exception _ -> true
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
            | Head, EmptyList -> Exception(HeadOfEmptyList, term), true
            | Tail, ConsList(_, list) -> list, true
            | Tail, EmptyList -> Exception(TailOfEmptyList, term), true
            | Not, Bool b -> Bool(not b), true
            | CharToNum, Char c -> Num(int c), true
            | NumToChar, Num n -> Char(Convert.ToChar(n)), true
            | Empty, EmptyList -> Bool true, true
            | Empty, ConsList _ -> Bool false, true
            | Fst, Pair(l, _) -> l, true
            | Snd, Pair(_, r) -> r, true
            | _, _ -> App(rm, rn), has_reduced_m || has_reduced_n
        | _ -> App(rm, rn), has_reduced_m || has_reduced_n
    | Abs (x, t) -> Abs(x, t), false
    | NumOperation (l, r, op) ->
        let newL, has_reduced_l = reduce l
        let newR, has_reduced_r = reduce r
        match newL, newR with
        | Num n1, Num n2 ->
            match op with
            | Plus -> Num(n1 + n2), true
            | Minus -> Num(n1 - n2), true
            | Times -> Num(n1 * n2), true
            | Divide ->
                if n2 = 0 then
                    Exception(DivideByZero, NumOperation (l, r, op)), false
                else 
                    Num(n1 / n2), true
            | Mod ->
                if n2 = 0 then
                    Exception(DivideByZero, NumOperation (l, r, op)), false
                else 
                    Num(n1 % n2), true                
        | _ -> NumOperation(newL, newR, op), has_reduced_l || has_reduced_r
    | BoolOperation (l, r, op) ->
        let newL, has_reduced_l = reduce l
        let newR, has_reduced_r = reduce r
        match newL, newR with
        | Bool b1, Bool b2 ->
            match op with
            | And -> Bool(b1 && b2), true
            | Or -> Bool(b1 || b2), true
        | _ -> BoolOperation(newL, newR, op), has_reduced_l || has_reduced_r
    | ComparisonOperation (l, r, op) ->
        let newL, has_reduced_l = reduce l
        let newR, has_reduced_r = reduce r
            
        match newL, newR with
        | Num n1, Num n2 ->
            match op with
            | LessThan -> Bool(n1 < n2), true
            | LessThanOrEqual -> Bool(n1 <= n2), true
            | GreaterThan -> Bool(n1 > n2), true
            | GreaterThanOrEqual -> Bool(n1 >= n2), true
            | Equals -> Bool(n1 = n2), true
            | NotEquals -> Bool(n1 <> n2), true
        | _ -> ComparisonOperation(newL, newR, op), has_reduced_l || has_reduced_r
    | ConsList (l, r) ->
        let newL, has_reducedL = reduce l
        let newR, has_reducedR = reduce r
        
        ConsList(newL, newR), (has_reducedL || has_reducedR)
    | InternalOperation op -> InternalOperation op, false
    | IfThenElse (cond, tr, fs) ->
        let newCond, has_reducedCond = reduce cond

        match newCond with
        | Bool true -> tr, true
        | Bool false -> fs, true
        | _ -> IfThenElse(newCond, tr, fs), has_reducedCond
    | Fix (x, t) -> substitute_var t x (Fix(x, t)), true
    | Let (a, b, c) ->
        let newB = evaluate b
        substitute_var c a newB, true
    | Ref t ->
        let newT = evaluate t
        let newPointer = memory.push newT
        Pointer newPointer, false
    | Deref t ->
        let newT = evaluate t
        match newT with
        | Pointer n -> memory.at n, true
        | _ -> Deref newT, false
    | Assign (l, r) ->
        let newL = evaluate l
        let newR = evaluate r
        
        match newL with
        | Pointer n ->
            memory.set n newR
            newR, true
        | _ -> Assign(newL, newR), false
    | Exception _ -> term, false
    | TryWith (t, handler) ->
        let newT = evaluate t
        match newT with
        | Exception _ ->
            evaluate handler, true
        | _ -> newT, false
    | Pair (l, r) ->
        let newL, has_reducedL = reduce l
        let newR, has_reducedR = reduce r
        Pair(newL, newR), (has_reducedL || has_reducedR)
    | Raise e -> Exception (e, term), false
    | Var _ | Num _ | EmptyList | Bool _ | Pointer _ | Unit | Char _-> term, false
/// Fully evaluate a term, throwing an exception if it takes too long
and evaluate term =
    let mutable redT: Term = term
    let mutable should_reduce = true

    let stopWatch =
        Diagnostics.Stopwatch.StartNew()

    while should_reduce do
        if stopWatch.Elapsed.Seconds = 5 then
            raise (TimeoutException("Timed out after 5 seconds, looping evaluation."))

        let reduced, has_reduced = reduce redT
        redT <- reduced
        should_reduce <- has_reduced

    redT
