module Core.Term

open System
open Core.Operators.InternalOperator
open Core.Operators.BinaryOperators
open Core.NameFactory

/// Represents a term (ie an expression) in the language.
type Term =
    | Var of string // A variable
    | Num of int // A number
    | App of Term * Term // Application of a term to another term
    | Abs of string * Term // Abstraction of a term
    | NumOperation of Term * Term * NumOperator // Operations on numbers (addition, multiplication, etc.)
    | BoolOperation of Term * Term * BoolOperator // Operations on booleans (and, or, etc.)
    | ComparisonOperation of Term * Term * ComparisonOperator // Operations of comparison (equal, less than, etc.)
    | ConsList of Term * Term // Cons of a term to a list (term::list)
    | EmptyList // Empty list
    | InternalOperation of InternalOperator // Special Operations in the language (head, tail, fst, snd, ect.)
    | Bool of bool // A boolean
    | Char of char // A character
    | IfThenElse of Term * Term * Term // If term then term else term
    | Fix of string * Term // Fixpoint of a term
    | Let of string * Term * Term // Let term = term in term
    | Pointer of int // A pointer to a memory location 
    | Ref of Term // A reference to a term (ref term)
    | Deref of Term // Dereference of a term (deref term)
    | Assign of Term * Term // Assignment of a term to a term (term := term)
    | Unit // Unit 
    | Exception of MLException * Term // Exception AND the term that caused the exception. 
    | TryWith of Term * Term // Try term with term
    | Pair of Term * Term // Pair of terms (term, term)
    | Raise of MLException // Raise an exception (raise exception)
/// Represents an exception in the language that is thron either because there is an error in the code (like a divide by zero) or by the user using the raise keyword.
and MLException =
    | BaseException // The base exception that is thrown when there is an error in the code.
    | DivideByZero // The exception that is thrown when there is a divide by zero.
    | HeadOfEmptyList // The exception that is thrown when the head of an empty list is taken.
    | TailOfEmptyList // The exception that is thrown when the tail of an empty list is taken.
    | UnkownExceptionException // The exception that is thrown when an exception is raised that is not defined in the language.
    
/// Represents the memory buffer of the language for referencing and deferenecing.
type Memory() =
    /// The memory buffer.
    member val memory = Array.empty with get, set
    
    /// Get the Term at the given index.
    member this.at (index: int) =
        this.memory[index]
    
    /// Pushes a new Term to the memory buffer.
    member this.push (value: Term) =
        this.memory <- Array.append this.memory [|value|]
        this.memory.Length - 1
        
    /// Sets the value at the given index to the given Term.
    member this.set (index: int) (value: Term) =
        this.memory[index] <- value
        
    /// Pretty prints the memory buffer (FOR DEBUG ONLY).
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
/// Get a readable string representation of an exception for debug purposes, with more details.
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
    
/// Rename a variable name to a new variable name using a renaming function
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
    
/// Substitute a name of a variable in a term
let substitute_name term from changeTo =
    map_name term (fun x -> if x = from then changeTo else x)

/// Substitute a variable by a term in a term
let substitute_var term from changeTo =
    map_var
        term
        (fun x bo ->
            if x = from && not (List.contains x bo) then
                changeTo
            else
                Var x)
        []

/// Converts a term to a new term with fresh names
/// At the end, ALL variables in the term will be unique and variables starting with @<name of var> are free
/// All variables in the form $<number> are bound
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
        // Since internal operations are defined internally and not in the grammar,
        // they do not count as variables IF they are not defnied as a variable (in an ABS for example)
        // This is why we check if the name is an internal operation by removing the @ (that means it is not a bound variable)
        // and then checking if it is an internal operation, if it is we return the Term of the internal operation
        // otherwise we return the variable as a free variable
        // Because nothing forbids the user to create such a term: fun head -> head, and here head is not the function head but just a var
        let internalOperator = internal_operator_of_string name
        
        match internalOperator with
        | Some op -> InternalOperation op
        | None -> Var x
    )) []
    
/// Returns the expansion of a term (Took it from the course)
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
/// Fully evaluate a term, throwing an exception if it takes too long (to prevent infinite loops)
/// This also means that if the term is CORRECT but takes too long to evaluate, it will throw an exception
/// This is a limitation of the current implementation but after asking you (the teacher) you said it was fine
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
