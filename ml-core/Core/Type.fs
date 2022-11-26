module Core.Type

open System.Collections.Generic
open System.Data
open Core.Term
open Exceptions.Errors
open Core.Operators.InternalOperator
open Core.NameFactory
open Microsoft.FSharp.Core

/// Represents the type of a term in the language.
type Type =
    | TVar of string // <var>
    | TArr of Type * Type // <fun>
    | TList of Type // List<T>
    | TNum // int
    | TChar // char
    | TBool // bool
    | TUnit // unit
    | TPair of Type * Type // Pair<T1, T2>
    | TPolymorphic of string list * Type // Forall<T1, T2, ..., Tn>.T
    | TPointer of Type // Pointer<T>

/// The environment of types used to infer the type of a term. Contains a target type that is our final type for the term.
type Env = (string * Type) list

/// The list of equations that are generated during the type inference process.
type Equations = (Type * Type) list

/// Get a readable string representation of a type, with more details than the default one for debugging purposes.
let rec string_of_type_debug t =
    match t with
    | TNum -> "num"
    | TVar x -> x
    | TArr (t1, t2) ->
        "("
        + string_of_type_debug t1
        + " -> "
        + string_of_type_debug t2
        + ")"
    | TList t -> "List<" + string_of_type_debug t + ">"
    | TBool -> "bool"
    | TUnit -> "unit"
    | TPair (t1, t2) -> $"pair<{string_of_type_debug t1},{string_of_type_debug t2}>"
    | TPolymorphic (vars, t) ->
        "forall "
        + String.concat " " vars
        + ". "
        + string_of_type_debug t
    | TPointer t -> "Pointer<" + string_of_type_debug t + ">"
    | TChar -> "char"
/// Get a readable string representation of a type but with alpha-renamed types (looks nice but not very useful for deubgging).
let string_of_type t =
    let namesMap = Dictionary<string, string>()
    let name_generator = fresh_var_alphabetic_generator() 
    
    let rec internal_string_of_type t =
        match t with
        | TNum -> "num"
        | TVar x ->
            if namesMap.ContainsKey(x) then
                namesMap[x]
            else
                let name = name_generator()
                namesMap[x] <- name
                name
        | TArr (t1, t2) ->
            "("
            + internal_string_of_type t1
            + " -> "
            + internal_string_of_type t2
            + ")"
        | TList t -> "List<" + internal_string_of_type t + ">"
        | TBool -> "bool"
        | TUnit -> "unit"
        | TPolymorphic (vars, t) ->
            "forall "
            + String.concat " " vars
            + ". "
            + internal_string_of_type t
        | TPointer t -> "Pointer<" + internal_string_of_type t + ">"
        | TChar -> "char"
        | TPair (t1, t2) -> $"pair<{internal_string_of_type t1},{internal_string_of_type t2}>"
    t |> internal_string_of_type

/// Pretty print a type
let pretty_print_type t = printfn $"%s{string_of_type_debug t}"

/// Get a readable string representation of an equation (for debugging purposes).
let pretty_print_equation eq =
    for t1, t2 in eq do
        printfn $"%s{string_of_type_debug t1} = %s{string_of_type_debug t2}\n"

/// Find the type of a variable in an environement or raise an error if it is not found.
let rec find_type var env =
    match env with
    | [] -> raise (System.MissingFieldException($"Type of Variable {var} not found in env: {env}"))
    | (v, t) :: _ when v = var -> t
    | (_, _) :: remaining -> (find_type var remaining)

/// Check if a variable is a type
let rec type_contains_var var t =
    match t with
    | TVar v -> v = var
    | TArr (t1, t2) -> (type_contains_var var t1) || (type_contains_var var t2)
    | TList t -> type_contains_var var t
    | TPolymorphic (_, t) -> type_contains_var var t
    | TPointer t -> type_contains_var var t
    | TPair (t1, t2) -> (type_contains_var var t1) || (type_contains_var var t2)
    | TNum | TBool | TUnit | TChar -> false

/// Check if an env contains a given type
let env_contains_var (env: Env) (var: string) =
    // printfn $"Checking if env:\n %O{env}\n contains var: \n%s{var}\n"
    env |> List.filter (fun (_, y) -> type_contains_var var y) |> List.length > 0
    // printfn $"Result: %b{res}\n"        

/// Map a type to the output of a function
let rec map_type t fn =
    match t with
    | TVar v -> fn v
    | TArr (t1, t2) -> TArr(map_type t1 fn, map_type t2 fn)
    | TList t -> TList(map_type t fn)
    | TPolymorphic (l, t) -> TPolymorphic(l, map_type t fn)
    | TPointer t -> TPointer(map_type t fn)
    | TPair (t1, t2) -> TPair(map_type t1 fn, map_type t2 fn)
    | TNum | TBool | TUnit | TChar -> t
/// Substitute a type for a variable
let substitute_type from changeTo in_t =
    map_type in_t (fun name ->
        if name = from then
            changeTo
        else
            TVar name)

/// Substitute a type for another  in an environment
let rec substitude_var_in_env (env: Env) (from: string) (changeTo: Type) =
    env |> List.map (fun (x, y) -> (x, substitute_type from changeTo y))

/// Substitutes an equation list
let substitute_eq (from: string) (changeTo: Type) (eq : Equations) : Equations =
    let mutable res: (Type * Type) list = []

    for t1, t2 in eq do
        res <-
            (substitute_type from changeTo t1, substitute_type from changeTo t2)
            :: res

    res
    
/// Open a type (change the forall to a variable)
let rec open_type t =
    match t with
    | TVar _ | TBool | TNum | TUnit | TChar -> t
    | TArr (t1, t2) -> TArr (open_type t1, open_type t2)
    | TList t -> TList (open_type t)
    | TPolymorphic (p_vars, t) ->
        let mutable t = t
        for p_var in p_vars do
            let new_name = name_factory()
            t <- substitute_type p_var (TVar new_name) t
        open_type t
    | TPointer t -> TPointer (open_type t)
    | TPair (t1, t2) -> TPair (open_type t1, open_type t2)

/// Gets the distinct variables (TVar) of a type    
let rec get_variables (t: Type) : string list =
    let rec internal_get_variables (t: Type) : string list =
        match t with
        | TVar v -> [v]
        | TArr (t1, t2) -> (internal_get_variables t1) @ (internal_get_variables t2)
        | TList t -> internal_get_variables t
        | TPointer t -> internal_get_variables t
        | TPair (t1, t2) -> (internal_get_variables t1) @ (internal_get_variables t2)
        | TPolymorphic (vars, t) ->
            internal_get_variables t
                |> List.filter (fun v -> not (List.contains v vars))
        | TNum | TBool | TUnit | TChar -> []
    internal_get_variables t |> List.distinct

/// Generalize a type
/// Wich is replacing all free type variables with a polymorphic type.
/// Example :
///    a -> b -> a becomes forall a b. a -> b -> a
let rec generalize (t: Type) (env: Env): Type =
    let mutable p_vars = []
    let mutable t = t
    let vars = get_variables t
    for var in vars do
        let is_in_env = env_contains_var env var
        if not is_in_env then
            let new_name = name_factory()
            let new_t = TVar(new_name)
            t <- substitute_type var new_t t
            p_vars <- new_name :: p_vars
            
    TPolymorphic(p_vars, t)

/// Generates the equations for a given term, with a given environment and on a given target type.
/// The environment is passed to the function to be able to update the environment with the new variables each time.
/// For example, if we have the term: Num 3, we know that the target is num and we have nothing to add to the environment.
/// But if we have the term: fun x -> x, we know that the target is a -> a, but we also need to add the variable x to the environment.
let rec generate_eq (term: Term) (target: Type) (env: Env) : Equations * Env =
    match term with
    | Var x ->
        let t = find_type x env
        [ (target, t) ], env
    | Num _ -> [ (target, TNum) ], env
    | Bool _ -> [ (target, TBool) ], env
    | Unit -> [ (target, TUnit) ], env
    | Char _ -> [ (target, TChar) ], env
    | Abs (x, t) ->
        let ta = name_factory ()
        let tr = name_factory ()
        let eq = TArr(TVar ta, TVar tr)
        let left_side = (target, eq)

        let resolved, _ =
            generate_eq t (TVar tr) ((x, TVar ta) :: env)

        (left_side :: resolved), env
    | App (lt, rt) ->
        let type_arg = TVar(name_factory ())
        let type_return = TVar(name_factory ())
        let leq, env = generate_eq lt (TArr(type_arg, type_return)) env
        let req, env = generate_eq rt type_arg env
        
        (leq @ req @ [ (target, type_return) ]), env
    | NumOperation (lt, rt, _) ->
        let leq, env = generate_eq lt TNum env
        let req, env = generate_eq rt TNum env
        
        (leq @ req @ [ (target, TNum) ]), env
    | BoolOperation (lt, rt, _) ->
        let leq, env = generate_eq lt TBool env
        let req, env = generate_eq rt TBool env
        
        (leq @ req @ [ (target, TBool) ]), env
    | ComparisonOperation (lt, rt, _) ->
        let leq, env = generate_eq lt TNum env
        let req, env = generate_eq rt TNum env
        
        (leq @ req @ [ (target, TBool) ]), env
    | ConsList(l, r) ->
        let type_el = TVar (name_factory())
        let type_tail = TList type_el
        
        let leq, env = generate_eq l type_el env
        let req, env = generate_eq r type_tail env
        let teq = (target, type_tail)

        (leq @ [teq;] @ req), env
    | EmptyList ->
        let new_var = name_factory()
        [(target, TList (TVar new_var))], env
    | InternalOperation op ->
        match op with
        | Head ->
            let name = name_factory()
            let type_op = TArr(TList (TVar name), TVar name)
            let eq = (target, type_op)

            [eq], env
        | Tail ->
            let name = name_factory()
            let type_op = TArr(TList (TVar name), TList (TVar name))
            let eq = (target, type_op)

            [eq], env
        | Not ->
            let type_op = TArr(TBool, TBool)
            let eq = (target, type_op)

            [eq], env
        | Empty ->
            let name = name_factory()
            let type_op = TArr(TList (TVar name), TBool)
            let eq = (target, type_op)

            [eq], env
        | NumToChar ->
            let type_op = TArr(TNum, TChar)
            let eq = (target, type_op)

            [eq], env
        | CharToNum ->
            let type_op = TArr(TChar, TNum)
            let eq = (target, type_op)

            [eq], env
        | Fst ->
            let name_left = name_factory()
            let name_right = name_factory()
            let type_op = TArr(TPair (TVar name_left, TVar name_right), TVar name_left)
            let eq = (target, type_op)

            [eq], env
        | Snd ->
            let name_left = name_factory()
            let name_right = name_factory()
            let type_op = TArr(TPair (TVar name_left, TVar name_right), TVar name_right)
            let eq = (target, type_op)

            [eq], env
    | IfThenElse (cond, tr, fs) ->
        let new_type = TVar (name_factory())
        let cond_eq, env = generate_eq cond TBool env
        let tr_eq, env = generate_eq tr new_type env
        let fs_eq, env = generate_eq fs new_type env
        let eq = (target, new_type)
        (cond_eq @ tr_eq @ fs_eq @ [eq]), env
    | Fix (x, t) ->
        let type_var = TVar (name_factory())
        let type_eq, _ = generate_eq t type_var ((x, type_var) :: env)
        let eq = (target, type_var)
        (eq :: type_eq), env
    | Let (a, b, c) ->
        let type_b, old_eqs, env = infer_type_with_env b env
        let mutable type_b = type_b
        
        if is_non_expansive b then
            type_b <- generalize type_b env
        
        let type_c = TVar(name_factory())
        let eq, env = generate_eq c type_c ((a, type_b) :: env)
        ([(target, type_c)] @ old_eqs @ eq), env
    | Pointer _ ->
        let type_eq = (target, TPointer (TVar (name_factory())))
        [type_eq], env
    | Ref t ->
        let type_t = TVar (name_factory())
        let type_eq, env = generate_eq t type_t env
        let eq = (target, TPointer type_t)
        (eq :: type_eq), env
    | Deref t ->
        let type_t = TVar (name_factory())
        let type_eq, env = generate_eq t (TPointer type_t) env
        let eq = (target, type_t)
        (eq :: type_eq), env
    | Assign (l, r) ->
        let type_l = TVar (name_factory())
        let type_r = TVar (name_factory())
        let type_eq_l, env = generate_eq l (TPointer type_l) env
        let type_eq_r, env = generate_eq r type_r env
        let eq = (type_l, type_r)
        let final = (target, TUnit)
        ([final] @ type_eq_l @ type_eq_r @ [eq]), env
    | Exception (_, t) ->
        let type_t = TVar (name_factory())
        let type_eq, env = generate_eq t type_t env
        let eq = (target, TUnit)
        (eq :: type_eq), env
    | TryWith (t, handler) ->
        let new_type = TVar (name_factory())
        let tr_eq, env = generate_eq t new_type env
        let fs_eq, env = generate_eq handler new_type env
        let eq = (target, new_type)
        
        (tr_eq @ fs_eq @ [eq]), env
    | Pair(l, r) ->
        let type_l = TVar (name_factory())
        let type_r = TVar (name_factory())
        let type_eq_l, env = generate_eq l type_l env
        let type_eq_r, env = generate_eq r type_r env
        let eq = (target, TPair (type_l, type_r))
        (eq :: type_eq_l @ type_eq_r), env
    | Raise _ ->
        let type_e = TVar (name_factory())
        
        [(target, type_e)], env
/// Does a step of unification and returns the new set of equations and an environment
and unify_one (eqs: Equations) (target: Type) (env: Env) : Equations * Env =
    let eqs = List.map (fun (a, b) -> (open_type a, open_type b)) eqs
    let res =
        eqs
        |> List.mapi (fun i eq -> (i, eq))
        |> List.filter (fun (_, (t1, t2)) -> t1 <> target && t2 <> target)
        |> List.take 1
        |> List.map (fun (i, eq) ->
            (match eq with
             | l, r when l = r -> (i, [], [])
             | TVar v, t
             | t, TVar v ->
                 if type_contains_var v t then
                     raise RecursiveTypeException
                 else
                     (i, [], [ (v, t) ])
             | TArr (l1, r1), TArr (l2, r2) -> (i, [ (l1, l2); (r1, r2) ], [])
             | TPair (l1, r1), TPair (l2, r2) -> (i, [(l1, l2); (r1, r2)], [])
             | TList l, TList r  -> (i, [(l, r)], [])
             | TPointer l, TPointer r -> (i, [(l, r)], [])
             | l, r -> raise (InvalidExpressionException($"Unification failed between {string_of_type_debug l} and {string_of_type_debug r}"))))
        |> List.head
    let index, news, substs = res
    let eqs = List.removeAt index eqs
    let mutable eqs = eqs @ news
    let mutable env = env
    for from, changeTo in substs do
        eqs <- substitute_eq from changeTo eqs
        env <- substitude_var_in_env env from changeTo
    eqs, env
/// Unifies all equations until there is only 1 remaining (which would be the end result)
/// There is no timeout since theoretically it should always terminate, which means that there are no loops either it finds a type or it throws an exception
and unify (target: Type) (env: Env) (eqs: Equations) : Equations * Env =
    let mutable res, env = eqs, env
    while res.Length > 1 do
        let a, b = unify_one res target env
        res <- a
        env <- b
    res, env
and infer_type_with_env (term : Term) (env : Env) : Type * Equations * Env =
    let target = TVar (name_factory())
    let old_eqs, env = generate_eq term target env
    let unif_eqs, env = unify target env old_eqs
    let t1, t2 = List.head unif_eqs
    
    if t1 = target then
        (t2, old_eqs, env)
    else if t2 = target then
        (t1, old_eqs, env)
    else
        raise(UnkownTypeException)
/// Infers the type of a given term
let infer_type (term : Term) : Type =
    let a, _, _ = infer_type_with_env term []
    a