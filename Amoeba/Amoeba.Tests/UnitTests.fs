namespace Amoeba.Tests

open NUnit.Framework
open FsUnit
open Amoeba
open Amoeba.Solver

module ``Amoeba solver tests`` = 
    
    let testAmoeba () =
        let sols = [|
            0., [| 0.; 1. |]; 
            1., [| 2.; 5. |];
            2., [| 4.; 6. |]; |]
        { Dim = 2; Solutions = sols }

    [<Test>]    
    let ``Centroid validation``() = 

        let amoeba = testAmoeba ()
        centroid amoeba |> should equal [| 1.; 3.; |]

    [<Test>]
    let ``Replace validation`` () =
        
        let amoeba = testAmoeba ()
        let sub = 0.5, [| 42.; 42.; |]
        
        let updated = replace amoeba sub
        
        updated.Size |> should equal amoeba.Size

        updated.Solutions.[0] |> should equal amoeba.Solutions.[0]
        updated.Solutions.[1] |> should equal sub
        updated.Solutions.[2] |> should equal amoeba.Solutions.[1]

    [<Test>]
    let ``Reflected validation`` () =
        
        let centroid = [| 0.; 1.; |]
        let x        = [| 4.; 3.; |]
        reflected centroid x Default.Alpha |> should equal [| -4.; -1. |]

    [<Test>]
    let ``Expanded validation`` () =
        
        let centroid = [| 0.; 1.; |]
        let x        = [| 4.; 3.; |]
        expanded centroid x Default.Gamma |> should equal [| -8.; -3. |]

    [<Test>]
    let ``Contracted validation`` () =
        
        let centroid = [| 0.; 1.; |]
        let x        = [| 4.; 3.; |]
        contracted centroid x Default.Rho |> should equal [| 2.; 2. |]

    [<Test>]
    let ``Shrink validation`` () =

        let amoeba = testAmoeba ()
        let f (p:Point) = 42.

        let updated = shrink amoeba f Default.Sigma

        snd updated.Solutions.[0] |> should equal (snd amoeba.Solutions.[0])
        snd updated.Solutions.[1] |> should equal [| 1.; 3.; |]
        snd updated.Solutions.[2] |> should equal [| 2.; 3.5|]
