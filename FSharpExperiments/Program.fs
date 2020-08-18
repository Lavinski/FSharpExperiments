// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    printfn "It begins!"

    AvoidingPrimitiveObsession.run ()

    Console.ReadLine() |> ignore
    0 // return an integer exit code
