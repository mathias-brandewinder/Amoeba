#load "Amoeba.fs"

open Amoeba
open Amoeba.Solver

(*
See the following article for more on the tested function:
http://msdn.microsoft.com/en-us/magazine/dn201752.aspx
The expected optimum is (0.0, [| 1.0; 1.0; |])
*)
let test () =

    let g (x:float) y =
        100. * pown (y - x * x) 2 + pown (1. - x) 2

    let testFunction (x:Point) =
        g x.[0] x.[1]

    solve Default [| (-10.,10.); (-10.,10.) |] testFunction 1000

(*
See the following page for more on the Beale function: 
http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO_files/Page288.htm
The expected optimum is (0.0, [| 3.0; 0.5; |])
*)
let bealeTest () =
    let beale x1 x2 = 
        pown (1.5-x1*(1.-x2)) 2 + 
        pown (2.25-x1*(1.- pown x2 2)) 2 +
        pown (2.625-x1*(1. - pown x2 3)) 2
    let f (p:Point) = beale p.[0] p.[1]
    let iter = 1000
    solve Default [| (-4.5,4.5); (-4.5,4.5) |] f iter

(*
See the following page form more on the Schwefel function:
http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO_files/Page2530.htm
Something looks wrong about the function itself, which might explain
why the solver fails so hard on it.
The expected optimum is (0.0, [| 1.0; 1.0; |]), but f doesn't give
that value at 1.0, 1.0, so something is clearly off.
*)
let schwefelTest () =
    
    let s (x:float) = 418.9829 - (x * sin (sqrt(abs(x))))
    let f (p:Point) = p |> Array.sumBy s

    let iter = 100
    solve Default [| (-500.,500.); (-500.,500.) |] f iter