module Core.Type

open Core.Term
open Exceptions.Errors

type Type =
    | TVar of string
    | TArr of Type * Type
    | TList of Type    
    | TNum

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

/// Pretty print a type
let pretty_print_type t = printfn $"%s{string_of_type t}"

/// Get a readable string representation of an equation
let pretty_print_equation eq =
    for t1, t2 in eq do
        printfn $"%s{string_of_type t1} = %s{string_of_type t2}\n"

/// Find the type of a variable in an environement
let rec find_type var env =
    match env with
    | [] -> raise (System.MissingFieldException($"Type of Variable {var} not found in {env}"))
    | (v, t) :: _ when v = var -> t
    | (_, _) :: remaining -> (find_type var remaining)

/// Resolve the type of a term
let rec generate_eq term target env =
    match term with
    | Var x ->
        let t = find_type x env
        [ (target, t) ]
    | Num _ -> [ (target, TNum) ]
    | Abs (x, t) ->
        let ta = name_factory ()
        let tr = name_factory ()
        let eq = TArr(TVar ta, TVar tr)
        let left_side = (target, eq)

        let resolved =
            generate_eq t (TVar tr) ((x, TVar ta) :: env)

        left_side :: resolved
    | App (lt, rt) ->
        let new_var = name_factory ()
        let leq = generate_eq lt (TArr(TVar new_var, target)) env
        let req = generate_eq rt (TVar new_var) env

        leq @ req
    | BinaryOperation (lt, rt, _) ->
        let leq = generate_eq lt TNum env
        let req = generate_eq rt TNum env
        
        leq @ req @ [ (target, TNum) ]
    | ConsList(l, r) ->
        let type_el = TVar (name_factory())
        let type_tail = TVar (name_factory())
        
        let leq = generate_eq l type_el env
        let req = generate_eq r type_tail env
        let teq = (target, type_tail)
        let final = (type_tail, TList type_el) 
        
        leq @ req @ [teq; final]
    | EmptyList ->
        let new_var = name_factory()
        [(target, TList (TVar new_var))]
    | Let (x, e1, e2) ->
        let new_var = name_factory ()
        let leq = generate_eq e1 (TVar new_var) env
        let req = generate_eq e2 target ((x, TVar new_var) :: env)

        leq @ req
        
/// Check if a variable is a type
let rec is_type var t =
    match t with
    | TVar v -> v = var
    | TArr (t1, t2) -> (is_type var t1) || (is_type var t2)
    | TNum -> false
    | TList t -> is_type var t

/// Map a type to the output of a function
let rec map_type t fn =
    match t with
    | TVar v -> fn v
    | TArr (t1, t2) -> TArr(map_type t1 fn, map_type t2 fn)
    | TNum -> t
    | TList t -> TList(map_type t fn)

/// Substitute a type for a variable
let sub_type from changeTo in_t =
    map_type in_t (fun name ->
        if name = from then
            changeTo
        else
            TVar name)


/// Substitute for an equation list
let sub_eq (from: string) (changeTo: Type) (eq : Equations) : Equations =
    let mutable res: (Type * Type) list = []

    for t1, t2 in eq do
        res <-
            (sub_type from changeTo t1, sub_type from changeTo t2)
            :: res

    res

/// Does a step of unification
let rec unify_one (eqs: Equations) (target: Type) : Equations =
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
                 if is_type v t then
                     raise RecursiveTypeException
                 else
                     (i, [], [ (v, t) ])
             | TArr (l1, r1), TArr (l2, r2) -> (i, [ (l1, l2); (r1, r2) ], [])
             | TList l, TList r  -> (i, [(l, r)], [])
             | l, r -> raise (TypeMismatchException($"Unification failed between %O{l} and %O{r}"))))
        |> List.head
    let index, news, substs = res
    let eqs = List.removeAt index eqs
    let eqs = eqs @ news
    let eqs = List.fold (fun eq (from, changeTo) -> sub_eq from changeTo eq) eqs substs
    eqs


/// Unifies all equations until there is only 1 remaining (which would be the end result)
let unify (target: Type) (eqs: Equations)  : Equations =
    let mutable res: Equations = eqs
    while res.Length > 1 do
        res <- unify_one res target
    res

/// Infers the type of a given term
let infer_type (term : Term) : Type =
    let target = TVar "Y"
    
    let t1, t2 = generate_eq term target []
              |> unify target
              |> List.head 
    
    if t1 = target then
        t2
    else if t2 = target then
        t1
    else
        raise(UnkownTypeException)
