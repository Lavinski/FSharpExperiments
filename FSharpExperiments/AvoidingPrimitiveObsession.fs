module AvoidingPrimitiveObsession

type Name = | Name of string

type PhoneNumber = | StreetNumber of string



let run () =
    let name1 = Name "Daniel"
    let name2 = Name "Toni"
    printfn "%A = %A |> %O" name1 name2 (name1 = name2)

