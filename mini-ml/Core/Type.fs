module Core.Type

open System.Data
open Core.Term
open Exceptions.Errors
open Core.Operators.InternalOperator

type Type =
    | TVar of string
    | TArr of Type * Type
    | TList of Type    
    | TNum
    | TBool
    | TPolymorphic of string list * Type

type Env = (string * Type) list

type Equations = (Type * Type) list

/// Get a readable string representation of a type
let rec string_of_type t =
    match t with
    | TNum -> "num"
    | TVar x -> x
    | TArr (t1, t2) ->
        "("
        + string_of_type t1
        + " -> "
        + string_of_type t2
        + ")"
    | TList t -> "List<" + string_of_type t + ">"
    | TBool -> "bool"
    | TPolymorphic (vars, t) ->
        "forall "
        + String.concat " " vars
        + ". "
        + string_of_type t
/// Pretty print a type
let pretty_print_type t = printfn $"%s{string_of_type t}"

/// Get a readable string representation of an equation
let pretty_print_equation eq =
    for t1, t2 in eq do
        printfn $"%s{string_of_type t1} = %s{string_of_type t2}\n"

/// Find the type of a variable in an environement
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
    | TNum | TBool-> false

/// Map a type to the output of a function
let rec map_type t fn =
    match t with
    | TVar v -> fn v
    | TArr (t1, t2) -> TArr(map_type t1 fn, map_type t2 fn)
    | TList t -> TList(map_type t fn)
    | TPolymorphic (l, t) -> TPolymorphic(l, map_type t fn)
    | TNum | TBool -> t

/// Substitute a type for a variable
let substitute_type from changeTo in_t =
    map_type in_t (fun name ->
        if name = from then
            changeTo
        else
            TVar name)


/// Substitute for an equation list
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
    | TVar _ | TBool | TNum -> t
    | TArr (t1, t2) -> TArr (open_type t1, open_type t2)
    | TList t -> TList (open_type t)
    | TPolymorphic (p_vars, t) ->
        let mutable t = t
        for p_var in p_vars do
            let new_name = name_factory()
            t <- substitute_type p_var (TVar new_name) t
        open_type t

let rec get_variables (t: Type) : string list =
    let rec get_variables2 (t: Type) : string list =
        match t with
        | TVar v -> [v]
        | TArr (t1, t2) -> (get_variables2 t1) @ (get_variables2 t2)
        | TList t -> get_variables2 t
        | TPolymorphic (vars, t) ->
            get_variables2 t
                |> List.filter (fun v -> not (List.contains v vars))
        | TNum | TBool -> []
    get_variables2 t |> List.distinct

/// 
let rec generalize (t: Type) (_env: Env): Type =
    let mutable p_vars = []
    let mutable t = t
    let vars = get_variables t
    for var in vars do
        let is_in_env = true
        if is_in_env then
            let new_name = name_factory()
            let new_t = TVar(new_name)
            t <- substitute_type var new_t t
            p_vars <- new_name :: p_vars
            
    TPolymorphic(p_vars, t)

/// Resolve the type of a term
let rec generate_eq term target env =
    match term with
    | Var x ->
        let t = find_type x env
        [ (target, t) ]
    | Num _ -> [ (target, TNum) ]
    | Bool _ -> [ (target, TBool) ]
    | Abs (x, t) ->
        let ta = name_factory ()
        let tr = name_factory ()
        let eq = TArr(TVar ta, TVar tr)
        let left_side = (target, eq)

        let resolved =
            generate_eq t (TVar tr) ((x, TVar ta) :: env)

        left_side :: resolved
    | App (lt, rt) ->
        let type_arg = TVar(name_factory ())
        let type_return = TVar(name_factory ())
        let leq = generate_eq lt (TArr(type_arg, type_return)) env
        let req = generate_eq rt type_arg env
        
        leq @ req @ [ (target, type_return) ]
    | NumOperation (lt, rt, _) ->
        let leq = generate_eq lt TNum env
        let req = generate_eq rt TNum env
        
        leq @ req @ [ (target, TNum) ]
    | BoolOperation (lt, rt, _) ->
        let leq = generate_eq lt TBool env
        let req = generate_eq rt TBool env
        
        leq @ req @ [ (target, TBool) ]
    | ComparisonOperation (lt, rt, _) ->
        let leq = generate_eq lt TNum env
        let req = generate_eq rt TNum env
        
        leq @ req @ [ (target, TBool) ]
    | ConsList(l, r) ->
        let type_el = TVar (name_factory())
        let type_tail = TList type_el
        
        let leq = generate_eq l type_el env
        let req = generate_eq r type_tail env
        let teq = (target, type_tail)

        leq @ [teq;] @ req
    | EmptyList ->
        let new_var = name_factory()
        [(target, TList (TVar new_var))]
    | InternalOperation op ->
        match op with
        | Head ->
            let name = name_factory()
            let type_op = TArr(TList (TVar name), TVar name)
            let eq = (target, type_op)

            [eq]
        | Tail ->
            let name = name_factory()
            let type_op = TArr(TList (TVar name), TList (TVar name))
            let eq = (target, type_op)

            [eq]
        | Not ->
            let type_op = TArr(TBool, TBool)
            let eq = (target, type_op)

            [eq]
        | Empty ->
            let name = name_factory()
            let type_op = TArr(TList (TVar name), TBool)
            let eq = (target, type_op)

            [eq]
    | IfThenElse (cond, tr, fs) ->
        let new_type = TVar (name_factory())
        let cond_eq = generate_eq cond TBool env
        let tr_eq = generate_eq tr new_type env
        let fs_eq = generate_eq fs new_type env
        let eq = (target, new_type)
        cond_eq @ tr_eq @ fs_eq @ [eq]
    | Fix (x, t) ->
        let type_var = TVar (name_factory())
        let type_eq = generate_eq t type_var ((x, type_var) :: env)
        let eq = (target, type_var)
        eq :: type_eq
    | Let (a, b, c) ->
        let type_b, old_eqs = infer_type_with_env b env
        let type_b = generalize type_b env
        let type_c = TVar(name_factory())
        let eq = generate_eq c type_c ((a, type_b) :: env)
        [(target, type_c)] @ old_eqs @ eq
/// Does a step of unification
and unify_one (eqs: Equations) (target: Type) : Equations =
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
             | TList l, TList r  -> (i, [(l, r)], [])
             
             | l, r -> raise (InvalidExpressionException($"Unification failed between {string_of_type l} and {string_of_type r}"))))
        |> List.head
    let index, news, substs = res
    let eqs = List.removeAt index eqs
    let eqs = eqs @ news
    let eqs = List.fold (fun eq (from, changeTo) -> substitute_eq from changeTo eq) eqs substs
    eqs
/// Unifies all equations until there is only 1 remaining (which would be the end result)
and unify (target: Type) (eqs: Equations)  : Equations =
    let mutable res: Equations = eqs
    while res.Length > 1 do
        res <- unify_one res target
    res

and infer_type_with_env (term : Term) (env : Env) : Type * Equations =
    let target = TVar (name_factory())
    let old_eqs = generate_eq term target env
    let t1, t2 = old_eqs
              |> unify target
              |> List.head 
    
    if t1 = target then
        (t2, old_eqs)
    else if t2 = target then
        (t1, old_eqs)
    else
        raise(UnkownTypeException)
/// Infers the type of a given term
let infer_type (term : Term) : Type =
    let a, _ = infer_type_with_env term []
    a