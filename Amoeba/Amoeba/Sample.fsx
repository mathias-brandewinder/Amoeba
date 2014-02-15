#load "Amoeba.fs"

open Amoeba
open Amoeba.Solver

let g (x:float) y =
    100. * pown (y - x * x) 2 + pown (1. - x) 2

let testFunction (x:Point) =
    g x.[0] x.[1]

let test () =

    let f = testFunction
    let start = 
        [| [| -0.66; 5.43; |]; [| 3.15; -1.34|]; [| -5.03; -7.79 |] |]
        |> Array.map (evaluate f)
        |> Array.sortBy fst

    let amoeba = { Dim = 2; Solutions = start }
    let rec search i a =
        if i > 0 then search (i-1) (update a f Default)
        else a.Solutions.[0]

    let iter = 50
    search iter amoeba

let schwefelTest () =
    
    let s (x:float) = -x * sin(sqrt(abs(x)))
    let f (p:Point) = p |> Array.sumBy s

    let iter = 100
    solve 2 3 Default f iter