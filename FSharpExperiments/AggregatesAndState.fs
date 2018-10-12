module AggregatesAndState

let o = obj()

type AggregateCommand =
    | Create of obj
    | Update of obj
    | Delete of obj
    //| Restore of obj

type AggregateEvent =
    | Created of obj
    | Updated of obj
    | Deleted of obj

[<RequireQualifiedAccess>]
type State =
    | Identity
    | Created
    | Updated
    | Deleted

// behaviour
let create payload state =
    Created payload
// apply
let created event state =
    State.Created


let update payload state =
    Updated payload

let updated event state =
    State.Updated


let delete payload state =
    Deleted payload

let deleted event state =
    State.Deleted


let handle2 command state =
    match state with
    | State.Identity ->
        match command with
        | Create c -> create state c
        | _ -> failwithf "Command %A could not be handled in state %A" command state
    | State.Created ->
        match command with
        | Update c -> update state c
        | _ -> failwithf "Command %A could not be handled in state %A" command state
    | State.Updated ->
        match command with
        | Delete c -> delete state c
        | _ -> failwithf "Command %A could not be handled in state %A" command state
    | State.Deleted ->
        failwithf "Command %A could not be handled in state %A" command state

let handle command state =
    match state, command with
    | State.Identity, Create payload -> create payload state
    | State.Created, Update payload -> update payload state
    | State.Updated, Delete payload -> delete payload state
    | _, Create _
    | _, Update _
    | _, Delete _ ->
        failwithf "Command %A could not be handled in state %A" command state

let apply state event =
    match event with
    | Created c -> created state c
    | Updated c -> updated state c
    | Deleted c -> deleted state c

let run () =
    let flip f x y = f y x

    let finalState =
        let apply = flip apply
        State.Identity
            |> apply (Created o)
            |> apply (Updated o)
            |> apply (Deleted o)

    printfn "Final State after apply: %A" finalState

    let happen command state =
        handle command state |> apply state

    let finalState =
        State.Identity
            |> happen (Create o)
            |> happen (Update o)
            |> happen (Delete o)
            |> happen (Delete o)

    printfn "Final State after handle: %A" finalState


//Apply(this, event)

