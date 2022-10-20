let rec fact n =
    if n == 0 then
        1
    else
        n * fact (n - 1)
in

let add a b = a + b in
let mul a b = a * b in

let cons e l = e :: l in

let rec map f list =
    if empty list then
        []
    else
        f (head list) :: map f (tail list)
in

let rec filter p list =
    if empty list then
        []
    else if p (head list) then
        head list :: filter p (tail list)
    else
        filter p (tail list)
in

let rec length list =
    if empty list then
        0
    else
        1 + length (tail list)
in

let rec concat a b =
    if empty a then
        b
    else
        head a :: concat (tail a) b
in

let rec range n =
    if n < 1 then
        []
    else
        concat (range (n - 1)) [n - 1]
in

let rec foldleft op init list =
    if empty list then
        init
    else
        foldleft op (op (head list) init) (tail list)
in 

let sum = foldleft add 0 in

let prod = foldleft mul 1 in

let compose f g x = f (g x) in
