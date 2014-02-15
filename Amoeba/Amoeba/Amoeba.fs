namespace Amoeba

type Point = float []
type Solution = float * Point
type Objective = Point -> float

module Solver = 

    type Amoeba = 
        { Dim:int; Solutions:Solution [] } // assumed to be sorted by fst value
        member this.Size = this.Solutions.Length
        member this.Best = this.Solutions.[0]
        member this.Worst = this.Solutions.[this.Size - 1]

    type Settings = { Alpha:float; Sigma:float; Gamma:float; Rho:float }

    let print (a:Amoeba) = 
        printfn "Amoeba state"
        a.Solutions 
        |> Seq.iter (fun (v,x) -> 
            printfn "  %.2f, %s" v (x |> Seq.map string |> String.concat ","))

    let Default = { Alpha=1.0; Sigma=0.5; Gamma=2.0; Rho=(-0.5) }

    let evaluate (f:Objective) (x:Point) = f x, x
    let valueOf (s:Solution) = fst s

    let replace (a:Amoeba) (s:Solution) = 
        let last = a.Size - 1
        let a' = Array.copy a.Solutions
        a'.[last] <- s
        { a with Solutions = a' |> Array.sortBy fst }

    let centroid (a:Amoeba) = 
        [| for d in 0 .. (a.Dim - 1) -> (a.Solutions.[0..a.Size - 2] |> Seq.averageBy(fun (_,x) -> x.[d])) |]

    let transform (c:Point) (x:Point) (s:float) =
        Array.map2 (fun c x -> c + s * (c - x)) c x

    let reflected (c:Point) (x:Point) alpha =
        Array.map2 (fun c x -> c + alpha * (c - x)) c x

    let expanded (c:Point) (x:Point) gamma =
        Array.map2 (fun c x -> c + gamma * (c - x)) c x

    let contracted (c:Point) (x:Point) rho =
        Array.map2 (fun c x -> c + rho * (c - x)) c x

    let shrink (a:Amoeba) (f:Objective) sigma =
        let best = snd a.Solutions.[0]
        { a with Solutions =         
                    a.Solutions 
                    |> Array.map (fun p -> Array.map2 (fun x b -> b + sigma * (x - b)) (snd p) best)
                    |> Array.map (evaluate f) } 

    let update (a:Amoeba) (f:Objective) (s:Settings) =
        let cen = centroid a
        let rv,r = reflected cen (snd a.Worst) s.Alpha |> evaluate f
        if ((valueOf (a.Best) <= rv) && (rv < (valueOf (a.Solutions.[a.Size - 2])))) then
            replace a (rv,r)
        else
            if (rv < valueOf (a.Best)) then
                let ev,e = expanded cen r s.Gamma |> evaluate f
                if (ev < rv) then
                    replace a (ev,e)
                else
                    replace a (rv,r)
            else
                let (cv,c) = contracted cen (snd a.Worst) s.Rho |> evaluate f
                if (cv < valueOf (a.Worst)) then
                    replace a (cv,c)
                else
                    shrink a f s.Sigma

    let solve dim size settings f iter =

        let rng = System.Random()
        let start = 
            [| for i in 1 .. size -> [| for x in 1 .. dim -> rng.NextDouble() |] |]
            |> Array.map (evaluate f)
            |> Array.sortBy fst
        let amoeba = { Dim = dim; Solutions = start }

        let rec search i a =
            if i > 0 then search (i-1) (update a f settings)
            else a.Solutions.[0]

        search iter amoeba